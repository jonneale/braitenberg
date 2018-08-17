(ns braitenberg.core)

(def initial-frame-rate 1)

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

(defn triangle-points
  [x y attitude sensor-width]
  [[x y]
   [(- x (/ sensor-width 2))
    (+ y sensor-width)]
   [(+ x (/ sensor-width 2))
    (+ y sensor-width)]])

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
          [(- x modifier) (- y modifier)]
          [(+ x modifier) (- y modifier)]])))

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

(defmulti position
  (fn [placement]
    placement))

(defmethod position :front-left
  [_]
  (fn [x y atttitude axle-width]
    [(- x (/ axle-width 2.0))
     (- y (/ axle-width 2.0))]
    [x y]))

(defmethod position :front-right
  [_]
  (fn [{:keys [x y attitude axle-width]}]
    [(+ x (/ axle-width 2.0))
     (- y (/ axle-width 2.0))]
    [x y]))


(defn attractor
  [id position-fn]
  (fn
    [state]
    (let [other-vehicles (remove (partial is-this-vehicle? id) (:vehicles state))]
      (if (some (partial occludes? (position-fn state)) other-vehicles)
        (do (println "Attracted!")
            1.0)
        0))))

(defn vehicle
  []
  (let [axle-width 0.02
        x (rand)
        y (rand)
        id (swap! id-counter inc)
        attitude (rand)
        sensor-width 0.2]
    {:id id
     :x x
     :y y
     :left-wheel-speed  -0.5
     :right-wheel-speed -0.3
     :left-sensors  [(make-sensor id
                                  (sensor-area :front-left x y axle-width sensor-width attitude)
                                  attractor)]
     :right-sensors [(make-sensor id
                                  (sensor-area :front-right x y axle-width sensor-width attitude)
                                  attractor)]
     :axle-width axle-width
     :attitude   attitude
     :sensor-width sensor-width
     :detectable-radius 0.3}))


(defn vehicle-1
  []
  (let [axle-width 0.02
        x 0.5
        y 1
        id (swap! id-counter inc)
        attitude 0
        sensor-width 0.2]
    {:id id
     :x x
     :y y
     :left-wheel-speed  0.5
     :right-wheel-speed 0.5
     :left-sensors  [(make-sensor id
                                  (sensor-area :front-left x y axle-width sensor-width attitude)
                                  attractor)]
     :right-sensors [(make-sensor id
                                  (sensor-area :front-right x y axle-width sensor-width attitude)
                                  attractor)]
     :axle-width axle-width
     :attitude   attitude
     :sensor-width sensor-width
     :detectable-radius 0.3}))

(defn vehicle-2
  []
  (let [axle-width 0.02
        x 0.5
        y 0
        id (swap! id-counter inc)
        attitude 0.5
        sensor-width 0.2]
    {:id id
     :x x
     :y y
     :left-wheel-speed  0.5
     :right-wheel-speed 0.5
     :left-sensors  [(attractor id (position :front-left))]
     :right-sensors [(make-sensor id
                                  (sensor-area :front-right x y axle-width sensor-width attitude)
                                  attractor)]
     :axle-width axle-width
     :attitude   attitude
     :sensor-width sensor-width
     :detectable-radius 0.3}))


(defn default-vehicle
  []
  ;; position can range from 0 to 1
  {:id 1
   :x 0.5
   :y 0.5
   :left-wheel-speed 0
   :right-wheel-speed 0
   :left-sensors  [wander]
   :right-sensors [wander]
   :axle-width 0.05
   :attitude 0.125
   :sensor-width 0.1
   :detectable-radius 0.3})

(defn reset-state
  []
  {:vehicles   [(vehicle-1) (vehicle-2)]
   :display-radius true
   :frame-rate initial-frame-rate})


