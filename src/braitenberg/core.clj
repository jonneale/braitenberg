(ns braitenberg.core)

(def initial-frame-rate 30)

(defn vehicle
  []
  ;; position can range from 0 to 1
  {:x (rand)
   :y (rand)
   :x-wheel-speed 0
   :y-wheel-speed 0
   :attitude (* (rand) 6)})

(defn reset-state
  []
  {:vehicles   (repeatedly 100 vehicle)
   :frame-rate initial-frame-rate})


