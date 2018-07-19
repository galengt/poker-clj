(ns poker_clj.core
  [:require [poker_clj.deal :refer [deck deal-hands play-one-hand]]
            [poker_clj.input :refer [get-int-input]]
            [poker_clj.output :refer [print-game]]]
  (:gen-class))

(defn play-poker
  []
  (let [deck (shuffle (deck))]
    (println "How many players?")
    (let [players (get-int-input 6)
          game (deal-hands players deck)]
      (play-one-hand game))))

(defn -main
  [& args]
  (println "Get ready to play poker!")
  (play-poker))


(comment

  (play-poker)

  (let [game (deal-hands 6 (shuffle (deck)))]
    (print-game game))

  )


; TODO to make multiround game
; add stack to hand, don't throw the hands away between rounds, build the stack based on winnings
; match a bet if someone bets higher in the same round
; Implement the "button", where does betting start

