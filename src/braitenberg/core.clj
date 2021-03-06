(ns braitenberg.core)

(def initial-frame-rate 60)

(defn to-radians
  [attitude]
  (* 2 Math/PI attitude))

(defn wander
  [state _]
  (/ (- (rand) 0.5) 10))

(defn calculate-change-in-position
  [speed attitude]
  (let [angle (to-radians attitude)]
    [(* speed (Math/sin angle))
     (- (* speed (Math/cos angle)))]))

(defn rotate-around-point
  [attitude [centre-x centre-y] [x y]]
  (let [[rel-x rel-y]    [(- x centre-x) (- y centre-y)]
        angle-in-radians (to-radians attitude)
        s                (Math/sin angle-in-radians)
        c                (Math/cos angle-in-radians)
        [new-x new-y]    [(- (* rel-x c) (* rel-y s))
                          (+ (* rel-x s) (* rel-y c))]]
    [(+ new-x centre-x) (+ new-y centre-y)]))

(defn- is-this-vehicle?
  [id-to-find {:keys [id]}]
  (= id id-to-find))

(defn negative?
  [[x1 y1] [x2 y2] [x3 y3]]
  (neg? (- (* (- x1 x3)
              (- y2 y3))
           (* (- x2 x3)
              (- y1 y3)))))

(defn square-bounding-box
  [x y attitude axle-width]
  (let [modifier (/ axle-width 2.0)]
    (map (partial rotate-around-point attitude [x y]) 
         [[(- x modifier) (+ y modifier)]
          [(+ x modifier) (+ y modifier)]
          [(+ x modifier) (- y modifier)]
          [(- x modifier) (- y modifier)]])))

(defn point-occludes?
  [[v1 v2 v3] [x y]]
  (let [plane-1-is-negative? (negative? [x y] v1 v2)
        plane-2-is-negative? (negative? [x y] v2 v3)
        plane-3-is-negative? (negative? [x y] v3 v1)]
    (= plane-1-is-negative? plane-2-is-negative? plane-3-is-negative?)))

(defn occludes?
  [triangle-points {:keys [x y attitude axle-width]}]
  (let [corners (square-bounding-box x y attitude axle-width)]
    (loop [remaining-corners corners]
      (when remaining-corners
        (let [[corner & rest] remaining-corners]
          (if (point-occludes? triangle-points corner)
            true
            (recur rest)))))))

(defn square
  [x]
  (* x x))

(def id-counter (atom 0))

(defn attitude-change
  [left-speed right-speed axle-width]
  (let [full-rotation-distance (* 2 (Math/PI) (* 0.5 axle-width))
        clockwise-rotation (/ left-speed full-rotation-distance)
        anticlockwise-rotation (/ right-speed full-rotation-distance)]
    (- clockwise-rotation anticlockwise-rotation)))

(defn calc-new-position
  [x y left-speed right-speed attitude axle-width cap-speed]
  (let [[adjusted-left-speed adjusted-right-speed] (map cap-speed [left-speed right-speed])
        combined-speed (/ (+ adjusted-left-speed adjusted-right-speed) 2)
        [new-x new-y]  (calculate-change-in-position combined-speed attitude)]
    [(+ x new-x) (+ y new-y) (+ attitude (attitude-change adjusted-left-speed adjusted-right-speed axle-width))]))

(defmulti sensed-area-offsets
  (fn [placement attitude sensor-width]
    placement))

(defmethod sensed-area-offsets :front-left
  [_ attitude axle-width]
  (rotate-around-point
   attitude
   [0 0]
   [(- (/ axle-width 2.0))
    (+ (/ axle-width 2.0))]))

(defmethod sensed-area-offsets :front-right
  [_ attitude axle-width]
  (rotate-around-point
   attitude
   [0 0]
   [(/ axle-width 2.0)
    (+ (/ axle-width 2.0))]))

(defn sensed-area
  [sensor-width sensor-height position {:keys [x y attitude axle-width]}]
  (let [[offset-x offset-y] (sensed-area-offsets position attitude axle-width)
        sensor-origin       [(+ offset-x x) (+ offset-y y)]
        [origin-x origin-y] sensor-origin]
    [sensor-origin
     (rotate-around-point attitude sensor-origin [(- origin-x (/ sensor-width 2))
                                                  (+ origin-y sensor-height)])
     (rotate-around-point attitude sensor-origin [(+ origin-x (/ sensor-width 2))
                                                  (+ origin-y sensor-height)])]))

(defn distance
  [v1 v2]
  (Math/sqrt (+ (square (- (:x v1)
                           (:x v2)))
                (square (- (:y v1)
                           (:y v2))))))

(defn wander
  [position & [sensor-width sensor-height]]
  (let [width  (or sensor-width 0)
        height (or sensor-height 0)
        rotate-every (* initial-frame-rate (rand-nth (range 1 20)))]
    {:position position
     :width    width
     :height   height
     :color    [0 255 0]
     :fn       (fn [vehicle state current-speed]
                 (if (zero? (rem (state :ticks) rotate-every))
                   (/ (- (rand) 1.0) 10.0)
                   current-speed))}))

(defn attractor
  [id position & [sensor-width sensor-height]]
  (let [width  (or sensor-width 0.7)
        height (or sensor-height 0.3)]
    {:position position
     :width    width
     :height   height
     :color    [255 0 0]
     :fn       (fn [vehicle state _]
                 (let [other-vehicles  (remove (partial is-this-vehicle? id) (:vehicles state))
                       sensed-vehicles (filter (partial occludes? (sensed-area width height position vehicle)) other-vehicles)]
                   (if (seq sensed-vehicles)
                     (- (reduce (fn [agg-attraction other-vehicle]
                                  (+ agg-attraction 1000))
                                0 sensed-vehicles))
                     0)))}))


#_(/ 1 (square
      (distance vehicle other-vehicle)))

(defn vehicle
  []
  (let [axle-width 0.1
        x (rand)
        y (rand)
        attitude (rand)]
    {:x x
     :y y
     :left-wheel-speed  (rand)
     :right-wheel-speed (rand)
     :axle-width axle-width
     :color [255 255 255]
     :attitude   attitude}))


(defn vehicle-1
  []
  (let [x 0.5
        y 0.45
        id (swap! id-counter inc)
        attitude 0.95
        sensor-width 0.5]
    (merge (vehicle)
           {:id  id
            :x x
            :y y
            :left-wheel-speed  0
            :right-wheel-speed 0
            :left-wheel-sensors  [(wander :front-right) (attractor id :front-right)]
            :right-wheel-sensors [(wander :front-left) (attractor id :front-left)]
            :color [0 255 0]
            :attitude   attitude})))

(defn vehicle-2
  []
  (let [x 0.5
        y 0.8
        id (swap! id-counter inc)
        attitude 0
        sensor-width 0.5]
    (merge (vehicle)
           {:id id
            :x x
            :y y
            :left-wheel-speed  0
            :right-wheel-speed 0
            :left-wheel-sensors  [(wander :front-left)  (attractor id :front-left)]
            :right-wheel-sensors [(wander :front-right) (attractor id :front-right)]
            :color [0 0 255]
            :attitude   attitude})))

(defn reset-state
  []
  {:vehicles   []
   :display-radius true
   :ticks      0
   :frame-rate initial-frame-rate})

