(ns poker.bet
  [:require [poker.input :refer [get-int-input]]])


(defn empty-rank-histogram
  [cards]
  (reduce (fn [histogram card]
            (assoc histogram (:rank card) 0))
          {}
          cards))

(defn empty-suit-histogram
  [cards]
  (reduce (fn [histogram card]
            (assoc histogram (:suit card) 0))
          {}
          cards))

(defn get-rank-histogram
  [cards]
  (reduce (fn [histogram card]
            (let [rank (:rank card)]
              (assoc histogram rank (+ 1 (rank histogram)))))
          (empty-rank-histogram cards)
          cards))

(defn get-suit-histogram
  [cards]
  (reduce (fn [histogram card]
            (let [suit (:suit card)]
              (assoc histogram suit (+ 1 (suit histogram)))))
          (empty-suit-histogram cards)
          cards))

(defn sequential-cards
  [cards]
  (let [sorted (sort-by :value cards)
        first-card (take 1 sorted)
        sequence {:last-value (:value first-card) :run 1 :long-run 1}]
    (let [long-run (:long-run (reduce (fn [sequence card]
                                        (let [card-value (:value card)
                                              current-run (:run sequence)
                                              long-run (:long-run sequence)]
                                          (cond (= (:last-value sequence) card-value)
                                                (assoc sequence :last-value card-value :run (+ 1 current-run) :long-run (max long-run (+ 1 current-run)))
                                                :else
                                                (assoc sequence :last-value card-value :run 1 :runs (concat (:runs sequence) [current-run])))))
                                      sequence
                                      (drop 1 sorted)))]
      long-run)))

(defn get-hand-rank-value
  [cards]
  (let [rank-histogram (get-rank-histogram cards)
        suit-hisogram (get-suit-histogram cards)
        histogram (reverse (sort (vals rank-histogram)))
        is-straght (> 4 (sequential-cards cards))
        is-flush (> 4 (apply max (vals suit-hisogram)))]

    ))

(defn get-bet-from-player
  [pot to-call]
  (println "Pot : " pot "To Call : " to-call)
  (println "What would you like to bet?")
  (get-int-input))


;value of best 5 cards (min 21, max 82)
;pair 100 x card value (min 200 + 15, max 1400 + 39)
;2 pair 300 x card + 300 x card (min 1500 + 4, max 8100 + 12)
;3 of a kind 5000 x card (min 10,000 + 7, max 70,000 + 25)
;straight 20000 x high card (min 100,000, max 280,000)
;flush 100000 x high card (min 500,000, max 1,400,000)
;full house (pair value + 3 of kind value) * 1000 (min 11,500,000 max 78,100,000)
;4 of a kind card value * 4 * 10,000,000 (min 80,000,000, max  560,000,000)
(defn get-bet
  [hand community pot to-call]
  ;(let [hand-value (+ (get-hand-rank-value (:cards hand) community) (:value hand))]
  ;  hand-value)
  (if (:is-human hand)
    (get-bet-from-player pot to-call)
    (max (:bet hand) (rand-int 20))))

(defn bet-one-round
  [game]
  (let [temp-game (assoc game :hands [])]
    (reduce (fn [game hand]
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
                    (assoc game :to-call bet :pot (+ pot bet) :hands (concat [(assoc hand :bet bet)] (:hands game)))
                  ; fold
                  (< bet to-call)
                    (assoc game :folded (concat [(assoc hand :bet bet)] (:folded game))))))
            temp-game
            (:hands game))))


(comment

  (get-sets
    [{:suit :hearts, :rank :king, :value 13} {:suit :diamonds, :rank :queen, :value 12}]
    [{:suit :diamonds, :rank :king, :value 13}
     {:suit :hearts, :rank :three, :value 3}
     {:suit :hearts, :rank :four, :value 4}])

  (empty-histogram [{:suit :hearts, :rank :king, :value 13} {:suit :diamonds, :rank :queen, :value 12}])
  (count (get-rank-histogram [{:suit :diamonds, :rank :king, :value 13}
                              {:suit :hearts, :rank :three, :value 3}
                              {:suit :hearts, :rank :four, :value 4}
                              {:suit :hearts, :rank :king, :value 13}
                              {:suit :diamonds, :rank :queen, :value 12}]))
  (let [hist (get-suit-histogram [{:suit :hearts, :rank :king, :value 13}
                                  {:suit :diamonds, :rank :queen, :value 12}
                                  {:suit :diamonds, :rank :queen, :value 12}
                                  {:suit :diamonds, :rank :queen, :value 12}
                                  {:suit :diamonds, :rank :queen, :value 12}
                                  {:suit :diamonds, :rank :king, :value 13}
                                  {:suit :hearts, :rank :three, :value 3}
                                  {:suit :hearts, :rank :four, :value 4}])]
    (apply max (vals hist)))

  (sequential-cards [
               {:suit :diamonds, :rank :queen, :value 12}
               {:suit :diamonds, :rank :queen, :value 12}
               {:suit :diamonds, :rank :king, :value 13}
               {:suit :hearts, :rank :three, :value 3}
               {:suit :hearts, :rank :four, :value 4}
                     {:suit :hearts, :rank :king, :value 13}
                     {:suit :diamonds, :rank :queen, :value 12}
                     {:suit :diamonds, :rank :jack, :value 11}])


  )
