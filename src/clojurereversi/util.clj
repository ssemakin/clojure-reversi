(ns clojurereversi.util)

(defn north-dir [[r c]] [(dec r) c])
(defn north-east-dir [[r c]] [(dec r) (inc c)])
(defn east-dir [[r c]] [r (inc c)])
(defn south-east-dir [[r c]] [(inc r) (inc c)])
(defn south-dir [[r c]] [(inc r) c])
(defn south-west-dir [[r c]] [(inc r) (dec c)])
(defn west-dir [[r c]] [r (dec c)])
(defn north-west-dir [[r c]] [(dec r) (dec c)])

(defn valid-pos? [sz [row column]]
    (and (> row 0) (> column 0) (<= row sz) (<= column sz)))

(def rival-color {:black :white, :white :black})

(defn print-board [board possible-moves]
  (print "  ")
  (loop [y 1]
    (when (<= y (board :size))
      (print y)
      (recur (inc y))))
  (println)
  (loop [y 1]
    (when (<= y (board :size))
      (print y "")
      (loop [x 1]
        (if (<= x (board :size))
          (do
            (cond
              (possible-moves [y x]) (print "*")
              (= (board [y x]) :black) (print "@")
              (= (board [y x]) :white) (print "0")
              :else (print "."))
            (recur (inc x)))
          (println)))
      (recur (inc y)))))

