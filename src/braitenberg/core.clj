(ns braitenberg.core)

(def initial-frame-rate 60)

(defn vehicle
  []
  ;; position can range from 0 to 1
  {:x (rand)
   :y (rand)
   :left-wheel-speed 0
   :right-wheel-speed 0
   :axle-width 0.02
   :attitude (rand)
   :detectable-radius 0.3})

(defn default-vehicle
  []
  ;; position can range from 0 to 1
  {:x 0.5
   :y 0.5
   :left-wheel-speed 0
   :right-wheel-speed 0
   :axle-width 0.05
   :attitude 0.25
   :detectable-radius 0.3})

(defn reset-state
  []
  {:vehicles   (repeatedly 10 vehicle)
   :display-radius true
   :frame-rate initial-frame-rate})


