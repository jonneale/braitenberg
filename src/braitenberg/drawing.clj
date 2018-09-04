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

(defn cap-speed-fn
  [frame-rate max-speed]
  (fn [speed]
    (/ (max (- max-speed) (min max-speed speed)) (* frame-rate 50.0))))

(defn new-speed
  [sensors vehicle state initial-speed]
  (let [updated-speed (reduce (fn [agg-speed sensor]
                                (+ agg-speed (sensor vehicle state))) 
                              initial-speed sensors)]
    (->> updated-speed
         (max (- max-speed))
         (min max-speed))))

(defn update-vehicle
  [state vehicle]
  (let [frame-rate (:frame-rate state)
        {:keys [x y left-wheel-speed right-wheel-speed attitude axle-width left-sensors right-sensors id]} vehicle
        [new-x new-y new-attitude]    (core/calc-new-position x y left-wheel-speed right-wheel-speed attitude axle-width (cap-speed-fn frame-rate max-speed))]
    (merge vehicle
           {:x (bound-position new-x)
            :y (bound-position new-y)
            :left-wheel-speed  (new-speed left-sensors vehicle state left-wheel-speed)
            :right-wheel-speed (new-speed right-sensors vehicle state right-wheel-speed)
            :attitude          new-attitude})))

(defn update-vehicles
  [state vehicles]
  (map (partial update-vehicle state) vehicles))

(defn update-state 
  [state]
  (update-in state [:vehicles] (partial update-vehicles state)))

(defn centre 
  [vehicle]
  [(- (/ (* width   (:axle-width vehicle)) 2.0))
   (- (/ (* height  (:axle-width vehicle)) 2.0))])

(defn- translate-coord
  [coord max-value]
  (min max-value (max 0 (* coord max-value))))

(defn to-coord
  [width height [x y]]
  [(translate-coord x width)
   (translate-coord y height)])

(defn display-sensors
  [vehicle]
  (let [w (translate-coord (:sensor-width vehicle) width)
        [centre-x centre-y] (centre vehicle)]
    (q/triangle 0.5 0.1 0.6 0.7 0.4 0.7)
    (q/no-fill)
    (q/stroke 0 0 255)
    (apply q/triangle (mapcat (partial to-coord width height) (core/sensed-area :front-left vehicle)))
    (apply q/triangle (mapcat (partial to-coord width height) (core/sensed-area :front-right vehicle)))
    (q/stroke 0 255 0)))

(defn round
  [n]
  (/ (int (* 100 n)) 100.0))

(defn draw-state [{:keys [vehicles frame-rate display-radius] :as state}]
  (q/background 255)
  (q/frame-rate frame-rate)
  (doseq [vehicle vehicles]
    (let [[centre-x centre-y]   (to-coord width height [(:x vehicle) (:y vehicle)])]
      (when display-radius (display-sensors vehicle))      
      (q/stroke 255 0 0)
      (apply q/quad (mapcat (partial to-coord width height) (apply core/square-bounding-box ((juxt :x :y :attitude :axle-width) vehicle)))))))


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
     :middleware [m/fun-mode m/pause-on-error]
     :key-pressed keyboard/key-pressed)))



