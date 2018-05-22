(ns poker.deal
  [:require [clojure.set :as set]
            [poker.bet :refer [bet-one-round]]])

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
    (assoc {} :cards cards :value score :bet 0 :is-human is-human)))

(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn print-card
  [card]
  (cond
    (or (= (:suit card) :hearts) (= (:suit card) :diamonds))
      (println (:rank card) " of " (colorize (:suit card) :red))
    :else
      (println (:rank card) " of " (colorize (:suit card) :reset))))

(defn print-hand
  [hand]
  (if (:is-human hand)
    (println "This is you"))
  (println "Cards:")
  (print-card (first (:cards hand)))
  (print-card (last (:cards hand)))
  (print "Bet: ")
  (println (:bet hand))
  (doseq [i (range 1 30)]
    (print "-"))
  (println))

(defn print-hands
  [hands]
  (println "Hands:")
  (println)
  (reduce (fn [foo hand]
               (print-hand hand))
             []
             hands))

(defn print-community
  [cards]
  (println "Community Cards:")
  (reduce (fn [foo card]
            (print-card card))
          []
          cards)
  (doseq [i (range 1 30)]
    (print "+-"))
  (println))

(defn print-bets
  [game]
  (println "Pot: ")
  (println (:pot game))
  (println "To Call: ")
  (println (:to-call game))
  (doseq [i (range 1 30)]
    (print "+-"))
  (println))

(defn print-game
  [game]
  (println)
  (println)
  (println)
  (println)
  (println)
  (print-community (:community game))
  (print-bets game)
  (print-hands (:hands game)))


(defn deal-hand
  [deck community num]
  [(hand (into [] (take 2 deck)) community num)
   (drop 2 deck)])

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
  (let [deck (:deck game)
        community (:community game)]
    (cond
      (= 0 (count community))
        ; flop
        (assoc game :community (concat (:community game) (take 3 deck)) :deck (drop 3 deck))
      :else
        ; turn and river
        (assoc game :community (concat (:community game) (take 1 deck)) :deck (drop 1 deck)))))

(defn bet-and-turn
  [game]
  (let [temp-game {:hands [] :community []  :pot 0 :to-call 0 :deck deck :folded []}]
    (reduce (fn [played-game round-num]
              ; TODO if only one hand left don't do another round
              (let [one-round-of-game (turn-community-card (prompt-bets played-game))]
                (assoc played-game
                  :hands (:hands one-round-of-game)
                  :community (:community one-round-of-game)
                  :pot (:pot one-round-of-game)
                  :to-call (:to-call one-round-of-game)
                  :folded (:folded one-round-of-game)
                  :deck (:deck one-round-of-game))))
            game
            (range 0 3))))

(comment
  (let [deck (shuffle (deck ranks suits))]
    (println "How many players?")
    (let [players (Integer. (get-input "6"))
          game (deal-hands players deck)]
      (bet-and-turn game)))



  )