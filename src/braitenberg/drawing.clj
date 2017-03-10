(ns braitenberg.drawing
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [braitenberg.keyboard :as keyboard]
            [braitenberg.core     :as core]))

(def width 500)
(def height 500)

(defn setup [initial-state]
  ; setup function returns initial state. It contains
  ; circle color and position.
  initial-state)

(def max-speed 0.05)

(defn update-attitude
  [ attitude]
  attitude)

(defn cap-speed
  [max-speed frame-rate speed]
  (/ (max (- max-speed) (min max-speed speed)) frame-rate))

(defn to-radians
  [attitude]
  (* 2 Math/PI attitude))

(defn distance-vector
  [speed attitude]
  [(* (Math/cos (to-radians attitude)) speed)
   (* (Math/sin (to-radians attitude)) speed)])

(defn attitude-change
  [left-speed right-speed axle-width]
  (let [full-rotation-distance (* 2 (Math/PI) (* 0.5 axle-width))
        clockwise-rotation (/ left-speed full-rotation-distance)
        anticlockwise-rotation (/ right-speed full-rotation-distance)]
    (- clockwise-rotation anticlockwise-rotation)))

(defn calc-new-position
  [x y left-speed right-speed attitude axle-width]
  (let [combined-speed (/ (+ left-speed right-speed) 2)
        [new-x new-y]  (distance-vector combined-speed attitude)]
    [(+ x new-x) (+ y new-y) (+ attitude (attitude-change left-speed right-speed axle-width))]))

(defn new-speed
  [initial-speed]
  (+ initial-speed (/ (- (rand) 0.5) 10)))

(defn update-vehicle
  [frame-rate vehicle]
  (let [{:keys [x y left-wheel-speed right-wheel-speed attitude axle-width]} vehicle
        [new-x new-y new-attitude]                                (calc-new-position x y left-wheel-speed right-wheel-speed attitude axle-width)]
    (merge vehicle
           {:x new-x
            :y new-y
            :left-wheel-speed  (cap-speed max-speed frame-rate (new-speed left-wheel-speed))
            :right-wheel-speed (cap-speed max-speed frame-rate (new-speed right-wheel-speed))
            :attitude          new-attitude})))

(defn update-vehicles
  [frame-rate vehicles]
  (map (partial update-vehicle frame-rate) vehicles))

(defn update-state [state]
  (update-in state [:vehicles] (partial update-vehicles (:frame-rate state))))

(defn- translate-coord
  [coord max-value]
  (min max-value (max 0 (* coord max-value))))

(defn draw-state [{:keys [vehicles frame-rate display-radius] :as state}]
                                        ; Clear the sketch by filling it with light-grey color.
  (q/background 255)
  (q/frame-rate frame-rate)
  (q/reset-matrix)
  (doseq [vehicle vehicles]
    (q/translate (+ (translate-coord (:x vehicle) width)
                    (/ (* width  (:axle-width vehicle)) 2.0)) 
                 (+ (translate-coord (:y vehicle) height)
                    (/ (* height  (:axle-width vehicle)) 2.0)))
    (q/rotate (to-radians (:attitude vehicle)))
    (when display-radius
      (do
        (q/stroke 0 255 0)
        (q/no-fill)
        (q/ellipse 0 0 
                   (translate-coord (:detectable-radius vehicle) width)
                   (translate-coord (:detectable-radius vehicle) width))))
    (q/stroke 0 0 0)
    (q/rect (- (/ (* width  (:axle-width vehicle)) 2.0))
            (- (/ (* height  (:axle-width vehicle)) 2.0))
            (* width  (:axle-width vehicle))
            (* height (:axle-width vehicle)))
    
    (q/reset-matrix)))

(defn run
  ([]
   (run (core/reset-state)))
  ([vehicles]
   (q/defsketch braitenberg
     :title "You spin my circle right round"
     :size [width height]
                                        ; setup function called only once, during sketch initialization.
     :setup #(setup vehicles)
                                        ; update-state is called on each iteration before draw-s
     :update update-state
     :draw   draw-state
     :features [:keep-on-top]
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
     :middleware [m/fun-mode]
     :key-pressed keyboard/key-pressed)))



