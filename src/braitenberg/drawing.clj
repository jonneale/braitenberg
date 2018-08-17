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
  [sensors state id initial-speed]
  (let [updated-speed (reduce (fn [agg-speed sensor]
                                (+ agg-speed (sensor state))) 
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
            :left-wheel-speed  (new-speed left-sensors state id left-wheel-speed)
            :right-wheel-speed (new-speed right-sensors state id right-wheel-speed)
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

(defn round
  [n]
  (/ (int (* 100 n)) 100.0))

(defn display-position
  [vehicle]
  (let [w (translate-coord (:sensor-width vehicle) width)
        [centre-x centre-y] (centre vehicle)]
    (q/fill 128 128 128)
    (q/text (str "["  (round (:x vehicle)) "," (round (:y vehicle)) "]") centre-x centre-y)
    (q/text (str (:attitude vehicle)) centre-x (+ 20 centre-y))
    (q/text (str (:left-wheel-speed vehicle)) (- centre-x 20) (+ 10 centre-y))
    (q/text (str (:right-wheel-speed vehicle)) (+ centre-x 20) (+ 10 centre-y))
    (q/no-fill)))

(defn draw-state [{:keys [vehicles frame-rate display-radius] :as state}]
  (q/background 255)
  (q/frame-rate frame-rate)
  (doseq [vehicle vehicles]
    (let [[centre-x centre-y] (centre vehicle)]
      (q/with-translation [(+ (translate-coord (:x vehicle) width)
                              centre-x) 
                           (+ (translate-coord (:y vehicle) height)
                              centre-y)]
        (q/with-rotation [(core/to-radians (:attitude vehicle))]
          (when display-radius (display-sensors vehicle))
          (when display-radius (display-position vehicle))
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



