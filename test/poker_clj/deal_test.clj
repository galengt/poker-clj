(ns poker-clj.deal-test
  (:require [clojure.test :refer :all]
            [poker_clj.deal :refer :all]))

(def test-deck [{:suit :spades, :rank :5, :value 5}
                {:suit :hearts, :rank :3, :value 3}
                {:suit :diamonds, :rank :3, :value 3}
                {:suit :clubs, :rank :3, :value 3}])
(def test-flop [{:suit :spades, :rank :2, :value 2}
                {:suit :hearts, :rank :5, :value 5}
                {:suit :diamonds, :rank :5, :value 5}])

(deftest dealt-hand
  (testing "One dealt hand"
    (is (= (:cards (first (deal-hand test-deck [] 1)))
           [{:suit :spades, :rank :5, :value 5} {:suit :hearts, :rank :3, :value 3}]))
    (is (= (:score (first (deal-hand test-deck [] 1)))
           8))
    (is (= (:bet (first (deal-hand test-deck [] 1)))
           0))
    (is (= (:num (first (deal-hand test-deck [] 1)))
           1))))


(deftest dealt-hands
  (testing "Deal 4 hands"
    (is (= (count (:hands (deal-hands 4 (deck))))
           4))
    (is (= (:community (deal-hands 4 (deck)))
           []))
    (is (= (:pot (deal-hands 4 (deck)))
           0))
    (is (= (:to-call (deal-hands 4 (deck)))
           0))
    (is (= (count (:deck (deal-hands 4 (deck))))
           44))))


(deftest flop
  (testing "No community cards, turn the river"
    (is (= (:community (turn-community-card {:deck test-deck :community []}))
           [{:suit :hearts, :rank :3, :value 3}
            {:suit :diamonds, :rank :3, :value 3}
            {:suit :clubs, :rank :3, :value 3}]))
    (is (= (:deck (turn-community-card {:deck test-deck :community []}))
           []))))

(deftest turn-or-river
  (testing "No community cards, turn the river"
    (is (= (:community (turn-community-card {:deck test-deck :community test-flop}))
           [{:suit :spades, :rank :2, :value 2}
            {:suit :hearts, :rank :5, :value 5}
            {:suit :diamonds, :rank :5, :value 5}
            {:suit :hearts, :rank :3, :value 3}]))
    (is (= (:deck (turn-community-card {:deck test-deck :community test-flop}))
           [{:suit :diamonds, :rank :3, :value 3}
            {:suit :clubs, :rank :3, :value 3}]))))