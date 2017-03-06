(ns braitenberg.drawing
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [braitenberg.keyboard :as keyboard]
            [braitenberg.core     :as core]))

(def width 500)
(def height 500)

(defn setup [initial-state]
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  initial-state)

(def max-speed 1.0)

(def speed-modifier 0.01)

(defn update-attitude
  [& [attitude]]
  attitude)

(defn cap-speed
  [max-speed speed]
  (max (- max-speed) (min max-speed speed)))

(defn update-vehicle
  [vehicle]
  (let [{:keys [x y x-wheel-speed y-wheel-speed attitude]} vehicle]
    {:x (+ x (* x-wheel-speed speed-modifier))
     :y (+ y (* y-wheel-speed speed-modifier))
     :x-wheel-speed (cap-speed max-speed (+ x-wheel-speed (- (rand) 0.5)))
     :y-wheel-speed (cap-speed max-speed (+ y-wheel-speed (- (rand) 0.5)))
     :attitude (update-attitude attitude x-wheel-speed y-wheel-speed)}))

(defn update-vehicles
  [vehicles]
  (map update-vehicle vehicles))

(defn update-state [state]
  (update-in state [:vehicles] update-vehicles))

(defn- translate-coord
  [coord max-value]
  (min max-value (max 0 (* coord max-value))))

(defn draw-state [{:keys [vehicles] :as state}]
                                        ; Clear the sketch by filling it with light-grey color.
  (q/background 255)
  (q/frame-rate (:frame-rate state))
  (doseq [vehicle vehicles]
    #_(println "["(translate-coord (:x vehicle) width)
               " " (translate-coord (:y vehicle) height) "]"
               " - speed [" 
               (:x-wheel-speed vehicle)
               " "
               (:y-wheel-speed vehicle)

               "]")
    (q/rect (translate-coord (:x vehicle) width)
            (translate-coord (:y vehicle) height)
            (/ width 100)
            (/ height 100))
    (q/rotate (:attitude vehicle))))

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



