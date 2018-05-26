(ns poker.input)

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input ""))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn get-int-input
  ([] (get-int-input 0))
  ([default]
   (Integer. (get-input (str default)))))