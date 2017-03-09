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

(defmethod key-pressed :default
  [state key]
  (println "Unknown key was pressed - " key)
  state)
