(ns braitenberg.core-test
  (:require [braitenberg.core :as sut]
            [clojure.test :as t :refer [deftest is]]))

(defn- abs [x] (if (zero? x) x (/ (* x x) x)))

(defn- close-enough?
  [target actual]
  (let [tolerance 0.00001]
    (every? (partial > tolerance) (map (comp abs -) target actual))))

(deftest rotate-around-point-works-for-origin-and-non-origin-rotations
  (is (close-enough? [(* -3 (Math/sqrt 2)) (* 3 (Math/sqrt 2))] (sut/rotate-around-point 0.125 [0 0] [0 6])))
  (is (close-enough? [-4 1] (sut/rotate-around-point 0.25 [0 0] [1 4])))
  (is (close-enough? [-3 0] (sut/rotate-around-point 0.75 [1 1] [1 4]))))

(deftest square-bounding-box-is-calculated-correctly
  (is (= [[0.0 2.0] [2.0 2.0] [0.0 0.0] [2.0 0.0]] (sut/square-bounding-box 1 1 0 2.0)))
  (is (= [[1.25 -0.25] [2.75 -0.25] [1.25 -1.75] [2.75 -1.75]] (sut/square-bounding-box 2 -1 0 1.5))))

(deftest two-points-are-marked-as-occluded-if-they-overlap
  (is (false? (sut/point-occludes? [[0 0] [4 0] [2 4]] [1.05 -0.05])))
  (is (false? (sut/point-occludes? [[0 0] [4 0] [2 4]] [2.95 -0.05])))
  (is (false? (sut/point-occludes? [[0 0] [4 0] [2 4]] [1.05 -1.95])))
  (is (false? (sut/point-occludes? [[0 0] [4 0] [2 4]] [2.95 -1.05])))
  (is (true? (sut/point-occludes? [[0 0] [4 0] [2 4]] [1 1])))
  (is (true? (sut/occludes? [[0 0] [4 0] [2 4]] {:x 2 :y 2 :attitude 0 :axle-width 2.0})))
  (is (not (true? (sut/occludes? [[0 0] [4 0] [2 4]] {:x 2 :y -1 :attitude 0 :axle-width 1.9})))))

(deftest object-is-marked-as-occluded-if-its-centre-is-outside-but-its-bounding-box-overlaps
  (is (true? (sut/occludes? [[0 0] [4 0] [2 4]] {:x 2 :y -1 :attitude 0 :axle-width 3.0}))))

(deftest object-is-marked-as-occluded-if-its-centre-is-outside-but-its-rotated-bounding-box-overlap
  (is (true? (sut/occludes? [[0 0] [4 0] [2 4]] {:x 2 :y -1 :attitude 0 :axle-width 3.0}))))

(deftest calc-new-position-works-one-tick
  (is (= [30.0 39.9 0.0]  (sut/calc-new-position 30 40 0.1 0.1 0 2.0 identity)))
  (is (= [30.0 40.1 0.5]  (sut/calc-new-position 30 40 0.1 0.1 0.5 2.0 identity)))
  (is (= [30.1 40.0 0.25]  (sut/calc-new-position 30 40 0.1 0.1 0.25 2.0 identity)))
  (is (close-enough? [30.0707 39.9292 0.125]  (sut/calc-new-position 30 40 0.1 0.1 0.125 2.0 identity))))


(deftest calculate-change-in-position-takes-angle-into-account
  (is (= [0.0 -0.1] (sut/calculate-change-in-position 0.1 0)))
  (is (close-enough? [0.0 -0.1] (sut/calculate-change-in-position 0.1 0.5)))
  (is (close-enough? [0.1 0.0] (sut/calculate-change-in-position 0.1 0.25)))
  (is (close-enough? [0.07071 -0.07071] (sut/calculate-change-in-position 0.1 0.125))))

(deftest sensed-area-is-triangle-extending-from-sensor-origin
  (is (= [[-2.0 -2.0] [-7.0 8.0] [3.0 8.0]] (sut/sensed-area :front-left 10 {:x 0 :y 0 :attitude 0 :axle-width 4}))))
