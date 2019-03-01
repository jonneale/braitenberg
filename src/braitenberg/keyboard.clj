(ns braitenberg.keyboard
  (:require [braitenberg.core :as core]))

(defmulti key-pressed
  (fn [_ key]
    (:key key)))

(defmethod key-pressed :f
  [state _]
  (let [new-state (update-in state [:frame-rate] (partial + 5))]
    (println "Going faster - frame rate is now " (:frame-rate new-state))
    new-state))

(defmethod key-pressed :s
  [state _]
  (let [new-state (update-in state [:frame-rate] (fn [frame-rate] (max 1 (- frame-rate 5))))]
    (println "Going slower - frame rate is now " (:frame-rate new-state))
    new-state))

(defmethod key-pressed :r
  [state _]
  (println "Resetting...")
  (core/reset-state))

(defmethod key-pressed :d
  [state _]
  (println "Current state: ")
  (println state)
  state)

(defmethod key-pressed :t
  [state _]
  (println "Toggling sensor lines")
  (update-in state [:display-radius] (partial not)))

(defmethod key-pressed :a
  [state _]
  (println "Adding bot: ")
  (update-in state [:vehicles] #(conj % (core/vehicle))))

(defmethod key-pressed :1
  [state _]
  (println "Adding bot: ")
  (update-in state [:vehicles] #(conj % (core/vehicle-1))))

(defmethod key-pressed :2
  [state _]
  (println "Adding bot: ")
  (update-in state [:vehicles] #(conj % (core/vehicle-2))))

(defmethod key-pressed :default
  [state key]
  (println "Unknown key was pressed - " key)
  state)
