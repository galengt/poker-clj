(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :refer :all]))


(deftest is-pair-in-hand
  (testing "Pair with community card"
    (is (get-sets
          [{:suit :hearts, :rank :king, :value 13}
           {:suit :diamonds, :rank :king, :value 12}]
          []))))

(deftest is-pair-test-with-community
  (testing "Pair with community card"
    (is (get-sets
          [{:suit :hearts, :rank :king, :value 13} {:suit :diamonds, :rank :queen, :value 12}]
          [{:suit :hearts, :rank :king, :value 13}
           {:suit :hearts, :rank :three, :value 3}
           {:suit :hearts, :rank :four, :value 4}]))))
