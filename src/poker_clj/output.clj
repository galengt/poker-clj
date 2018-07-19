(ns poker_clj.output)

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
  (println "Number:" (:number hand))
  (println "Cards:")
  (print-card (first (:cards hand)))
  (print-card (last (:cards hand)))
  (print "Bet: ")
  (println (:bet hand))
  (doseq [i (range 1 30)]
    (print "-"))
  (println))

(defn print-hands
  [hands type]
  (println type "Hands:")
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
  (print-hands (:hands game) "Active")
  (println)
  (print-hands (:folded game) "Folded"))
