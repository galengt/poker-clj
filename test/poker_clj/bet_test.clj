(ns poker-clj.bet-test
  (:require [clojure.test :refer :all]
            [poker_clj.bet :refer :all]))

(deftest no-streak
  (testing "Multiple cards of same type don't break streak"
    (is (sequential-cards
          [{:suit :diamonds, :rank :king, :value 13}
           {:suit :hearts, :rank :three, :value 3}
           {:suit :hearts, :rank :five, :value 4}
           {:suit :diamonds, :rank :jack, :value 11}])
        0)))

(deftest sort-card
  (testing "Multiple cards of same type don't break streak"
    (is (sequential-cards
          [{:suit :diamonds, :rank :king, :value 13}
           {:suit :hearts, :rank :three, :value 3}
           {:suit :hearts, :rank :four, :value 4}
           {:suit :spades, :rank :queen, :value 12}
           {:suit :diamonds, :rank :jack, :value 11}])
        3)))

(deftest pairs-streak
  (testing "Multiple cards of same type don't break streak"
    (is (sequential-cards
          [{:suit :diamonds, :rank :queen, :value 12}
           {:suit :hearts, :rank :queen, :value 12}
           {:suit :diamonds, :rank :king, :value 13}
           {:suit :hearts, :rank :three, :value 3}
           {:suit :hearts, :rank :four, :value 4}
           {:suit :hearts, :rank :king, :value 13}
           {:suit :spades, :rank :queen, :value 12}
           {:suit :diamonds, :rank :jack, :value 11}])
        3)))

(deftest pairs-streak
  (testing "Multiple cards of same type don't break streak"
    (is (calculate-rank
          [{:suit :diamonds, :rank :queen, :value 12}
           {:suit :hearts, :rank :queen, :value 12}]
          [{:suit :diamonds, :rank :king, :value 13}
           {:suit :hearts, :rank :three, :value 3}
           {:suit :hearts, :rank :four, :value 4}])
        :pair)))