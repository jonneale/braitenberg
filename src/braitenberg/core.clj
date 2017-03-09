(ns braitenberg.core)

(def initial-frame-rate 1)

(defn vehicle
  []
  ;; position can range from 0 to 1
  {:x (rand)
   :y (rand)
   :left-wheel-speed 0
   :right-wheel-speed 0
   :axle-width 0.02
   :attitude (rand)})

(defn default-vehicle
  []
  ;; position can range from 0 to 1
  {:x 0.5
   :y 0.5
   :left-wheel-speed 0
   :right-wheel-speed 0
   :axle-width 0.05
   :attitude (rand)})

(defn reset-state
  []
  {:vehicles   (repeatedly 2 vehicle)
   :frame-rate initial-frame-rate})


