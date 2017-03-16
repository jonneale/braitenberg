(ns braitenberg.drawing
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [braitenberg.keyboard :as keyboard]
            [braitenberg.core     :as core]))

(def width 500)
(def height 500)

(def max-speed 0.5)

(defn bound-position
  [position]
  (min (max 0 position) width))

(defn cap-speed
  [max-speed frame-rate speed]
  (/ (max (- max-speed) (min max-speed speed)) (* frame-rate 50.0)))

(defn to-radians
  [attitude]
  (* 2 Math/PI attitude))

(defn distance-vector
  [speed attitude]
  [(* (Math/sin (to-radians attitude)) speed)
   (- (* (Math/cos (to-radians attitude)) speed))])

(defn attitude-change
  [left-speed right-speed axle-width]
  (let [full-rotation-distance (* 2 (Math/PI) (* 0.5 axle-width))
        clockwise-rotation (/ left-speed full-rotation-distance)
        anticlockwise-rotation (/ right-speed full-rotation-distance)]
    (- clockwise-rotation anticlockwise-rotation)))

(defn calc-new-position
  [x y left-speed right-speed attitude axle-width frame-rate]
  (let [[adjusted-left-speed adjusted-right-speed] (map (partial cap-speed max-speed frame-rate) [left-speed right-speed])
        combined-speed (/ (+ adjusted-left-speed adjusted-right-speed) 2)
        [new-x new-y]  (distance-vector combined-speed attitude)]    
    [(+ x new-x) (+ y new-y) (+ attitude (attitude-change adjusted-left-speed adjusted-right-speed axle-width))]))

(defn new-speed
  [initial-speed]
  (->> initial-speed
       (+ (/ (- (rand) 0.5) 10))
       (max (- max-speed))
       (min max-speed)))

(defn update-vehicle
  [frame-rate vehicle]
  (let [{:keys [x y left-wheel-speed right-wheel-speed attitude axle-width]} vehicle
        [new-x new-y new-attitude]    (calc-new-position x y left-wheel-speed right-wheel-speed attitude axle-width frame-rate)]
    (merge vehicle
           {:x (bound-position new-x)
            :y (bound-position new-y)
            :left-wheel-speed  (new-speed left-wheel-speed)
            :right-wheel-speed (new-speed right-wheel-speed)
            :attitude          new-attitude})))

(defn update-vehicles
  [frame-rate vehicles]
  (map (partial update-vehicle frame-rate) vehicles))

(defn update-state 
  [state]
  (update-in state [:vehicles] (partial update-vehicles (:frame-rate state))))

(defn centre 
  [vehicle]
  [(- (/ (* width   (:axle-width vehicle)) 2.0))
   (- (/ (* height  (:axle-width vehicle)) 2.0))])

(defn- translate-coord
  [coord max-value]
  (min max-value (max 0 (* coord max-value))))

(defn display-sensors
  [vehicle]
  (let [w (translate-coord (:sensor-width vehicle) width)
        [centre-x centre-y] (centre vehicle)]
    (q/no-fill)
    (q/stroke 255 0 0)
    (q/triangle (- 0 w centre-x) (- 0 w centre-y) 
                (+ (- 0 centre-x) w) (- 0 w centre-y) 
                (+ centre-x) (+ centre-y))
    (q/triangle (- 0 w (* 3 centre-x)) (- 0 w centre-y) 
                (+ (- 0 (* 3 centre-x)) w) (- 0 w centre-y) 
                (- 0 centre-x) (+ centre-y))
    (q/stroke 0 255 0)
    (q/ellipse 0 0 
               (translate-coord (:detectable-radius vehicle) width)
               (translate-coord (:detectable-radius vehicle) width))))

(defn draw-state [{:keys [vehicles frame-rate display-radius] :as state}]
  (q/background 255)
  (q/frame-rate frame-rate)
  (doseq [vehicle vehicles]
    (let [[centre-x centre-y] (centre vehicle)]
      (q/with-translation [(+ (translate-coord (:x vehicle) width)
                              centre-x) 
                           (+ (translate-coord (:y vehicle) height)
                              centre-y)]
        (q/with-rotation [(to-radians (:attitude vehicle))]
          (when display-radius (display-sensors vehicle))
          (q/stroke 0 0 0)
          (q/rect centre-x
                  centre-y
                  (* width  (:axle-width vehicle))
                  (* height (:axle-width vehicle))))))))


(defn run
  ([]
   (run (core/reset-state)))
  ([initial-state]
   (q/defsketch braitenberg
     :title "You spin my circle right round"
     :size [width height]
     :setup  (constantly initial-state)
     :update update-state
     :draw   draw-state
     :features [:keep-on-top]
     :middleware [m/fun-mode]
     :key-pressed keyboard/key-pressed)))



