(ns poker_clj.deal
  [:require [clojure.set :as set]
            [poker_clj.output :refer [print-game]]
            [poker_clj.input :refer [get-int-input]]])

(def suits [:hearts :diamonds :clubs :spades])
(def ranks {2 :2
            3 :3
            4 :4
            5 :5
            6 :6
            7 :7
            8 :8
            9 :9
            10 :10
            11 :jack
            12 :queen
            13 :king
            14 :ace})

;TODO add record for game and hand?
(defrecord Card [suit rank value])
(defprotocol Hand
  (get-bet [hand community pot to-call]))

(defrecord PlayerHand [cards score bet num]
  Hand
  (get-bet [x community pot to-call]
    (println "Pot : " pot "To Call : " to-call)
    (println "What would you like to bet?")
    (get-int-input)))

(defrecord ComputerHand [cards score bet num]
  Hand
  (get-bet [x community pot to-call]
    (max bet (rand-int 20))))


(defn card
  [value suit]
  (map->Card (assoc {} :suit suit :rank (ranks value) :value value)))

(defn deck
  []
  (into []
        (for [suit suits
                 n (range 2 15)]
             (card n suit))))

(defn score-hand
  [cards community]
  (+ (:value (first cards)) (:value (last cards))))

(defn hand
  [cards community num]
  (let [score (score-hand cards community)]
    (cond (= 1 num) (->PlayerHand cards score 0 num)
          :else (->ComputerHand cards score 0 num))))

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
              (let [[hand remaining-deck] (deal-hand (:deck game) (:community game) num)]
                (assoc game :hands (cons hand (:hands game)) :deck remaining-deck)))
            dealt-game
            (range 1 (inc num-players)))))

(defn can-turn?
  [game]
  (let [to-call (:to-call game)]
    (every? #(= (:bet %) to-call) (:hands game))))

; this could reduce into (to-call [hand1, hand2, ...]) instead of game
(defn bet-one-round
  [game]
  (let [temp-game (assoc game :hands [])]
    (reduce (fn [game hand]
              (if (:is-human hand)
                (print-game game))
              (let [pot (:pot game)
                    to-call (:to-call game)
                    community (:community game)
                    current-bet (:bet hand)
                    bet (get-bet hand community pot to-call)]
                (cond
                  ; call
                  (and (= bet to-call) (not (= bet current-bet)))
                  (assoc game :pot (+ pot bet) :hands (concat [(assoc hand :bet bet)] (:hands game)))
                  ; already called
                  (and (= bet to-call) (= bet current-bet))
                  (assoc game :hands (concat [(assoc hand :bet bet)] (:hands game)))
                  ; raise
                  (> bet to-call)
                  ;TODO I think the raise in this case is (bet - current-bet), and that is what gets added to the pot
                  (assoc game :to-call bet :pot (+ pot bet) :hands (concat [(assoc hand :bet bet)] (:hands game)))
                  ; fold
                  (< bet to-call)
                  (assoc game :folded (concat [(assoc hand :bet bet)] (:folded game))))))
            temp-game
            (:hands game))))

(defn prompt-bets
  [game]
  (print-game game)
  (let [temp-game (bet-one-round game)]
    (if (not (can-turn? temp-game))
      (recur temp-game)
      temp-game)))

(defn turn-community-card
  [game]
  (let [deck (drop 1 (:deck game));burn
        community (:community game)
        cards-to-draw (cond
          (= 0 (count community))
          3;flop
          :else 1)] ;turn and river
      (assoc
        game
        :community (vec (concat (:community game) (take cards-to-draw deck)))
        :deck (vec (drop cards-to-draw deck)))))

(defn play-one-hand
  [game]
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
          (range 0 3)))
