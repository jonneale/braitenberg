(ns braitenberg.core)

(def initial-frame-rate 60)

(defn attractor
  []
  (fn [distance-to-target]
    distance-to-target))

(defn vehicle
  []
  ;; position can range from 0 to 1
  {:x (rand)
   :y (rand)
   :left-wheel-speed 0.001
   :right-wheel-speed 0.001
   :left-sensor  attractor
   :right-sensor attractor
   :axle-width 0.02
   :attitude (rand)
   :sensor-width 0.3
   :detectable-radius 0.3})

(defn default-vehicle
  []
  ;; position can range from 0 to 1
  {:x 0.5
   :y 0.5
   :left-wheel-speed 0.01
   :right-wheel-speed 0.01
   :axle-width 0.05
   :attitude 0.125
   :sensor-width 0.3
   :detectable-radius 0.3})

(defn reset-state
  []
  {:vehicles   (repeatedly 1 vehicle)
   :display-radius true
   :frame-rate initial-frame-rate})


