(ns clojurereversi.util)

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

