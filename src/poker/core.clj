(ns poker.core
  [:require [poker.deal :refer [deck deal-hands bet-and-turn print-game]]
            [poker.input :refer [get-int-input]]]
  (:gen-class))

(defn play-poker
  []
  (let [deck (shuffle (deck))]
    (println "How many players?")
    (let [players (get-int-input 6)
          game (deal-hands players deck)]
      (bet-and-turn game))))

(defn -main
  [& args]
  (println "Get ready to play poker!")
  (play-poker))


(comment

  (play-poker)

  (let [game (deal-hands 6 (shuffle (deck)))]
    (print-game game))

  )


; add stack to hand, don't throw the hands away between rounds, build the stack based on winnings
; match a bet if someone bets higher in the same round
; The button, where does betting start

