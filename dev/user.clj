(ns user)

(println "Ready to rock and roll")

(defn restart
  []
  (use 'braitenberg.keyboard :reload)
  (use 'braitenberg.core :reload)
  (use 'braitenberg.drawing :reload))
