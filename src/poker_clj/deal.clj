(ns poker_clj.deal
  [:require [clojure.set :as set]
            [poker_clj.bet :refer [bet-one-round]]
            [poker_clj.output :refer [print-game]]])

(def suits [:hearts :diamonds :clubs :spades])
(def ranks {:2 2
            :3 3
            :4 4
            :5 5
            :6 6
            :7 7
            :8 8
            :9 9
            :10 10
            :jack 11
            :queen 12
            :king 13
            :ace 14})

(defn card
  [rank suit]
  (assoc {} :suit suit :rank (key rank) :value (val rank)))

(defn suits-for-rank
  [rank suits]
  (reduce (fn [deck suit]
    (into deck [(card rank suit)]))
    []
    suits))

(defn deck
  []
  (reduce (fn [deck rank]
            (into deck (suits-for-rank rank suits)))
          []
          ranks))

(defn score-hand
  [cards community]
  (+ (:value (first cards)) (:value (last cards))))

(defn hand
  [cards community num]
  (let [score (score-hand cards community)
        is-human (= 1 num)]
    (assoc {} :cards cards :value score :bet 0 :is-human is-human :number num)))

; replace all of the (assoc game ...) with this?
(defn game
  [hands community pot to-call deck folded-hands]
  {:hands hands :community community :pot pot :to-call to-call :deck deck :folded folded-hands})

(defn deal-hand
  [deck community num]
  [(hand (into [] (take 2 deck)) community num)
   (drop 2 deck)])

(comment
  (deal-hands 4 (deck)))

(defn deal-hands
  [num-players deck]
  (let [dealt-game {:hands [] :community []  :pot 0 :to-call 0 :deck deck}]
    (reduce (fn [game num]
              (let [one-deal (deal-hand (:deck game) (:community game) num)]
                (assoc game :hands (concat [(first one-deal)] (:hands game)) :deck (last one-deal))))
            dealt-game
            (range 1 (inc num-players)))))

(defn can-turn?
  [game]
  (let [to-call (:to-call game)]
    (empty? (reduce (fn [test hand]
                      (if (< (:bet hand) to-call)
                        (into test "nope")))
                    []
                    (:hands game)))))
(defn prompt-bets
  [game]
  (print-game game)
  (let [temp-game (bet-one-round game)]
    (if (not (can-turn? temp-game))
      (prompt-bets temp-game)
      temp-game)))

(defn turn-community-card
  [game]
  (let [deck (drop 1 (:deck game));burn
        community (:community game)]
    (cond
      (= 0 (count community))
        ; flop
        (assoc game :community (vec (concat (:community game) (take 3 deck))) :deck (vec (drop 3 deck)))
      :else
        ; turn and river
        (assoc game :community (vec (concat (:community game) (take 1 deck))) :deck (vec (drop 1 deck))))))

(defn play-one-hand
  [game]
  (let [temp-game {:hands [] :community []  :pot 0 :to-call 0 :deck deck :folded []}]
    (reduce (fn [played-game round-number]
              (cond
                (= 1 (count (:hands played-game)))
                  played-game
                :else
                  (let [one-round-of-game (turn-community-card (prompt-bets played-game))]
                    (assoc played-game
                      :hands (:hands one-round-of-game)
                      :community (:community one-round-of-game)
                      :pot (:pot one-round-of-game)
                      :to-call (:to-call one-round-of-game)
                      :folded (:folded one-round-of-game)
                      :deck (:deck one-round-of-game)))))
            game
            ; pre-flop, flop, turn, river
            (range 0 3))))
