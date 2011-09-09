(ns clojurereversi.util)

(defn north-dir [[r c]] [(dec r) c])
(defn north-east-dir [[r c]] [(dec r) (inc c)])
(defn east-dir [[r c]] [r (inc c)])
(defn south-east-dir [[r c]] [(inc r) (inc c)])
(defn south-dir [[r c]] [(inc r) c])
(defn south-west-dir [[r c]] [(inc r) (dec c)])
(defn west-dir [[r c]] [r (dec c)])
(defn north-west-dir [[r c]] [(dec r) (dec c)])

;(defn valid-pos? [board pos]
;  (and (> (pos 0) 0) (> (pos 1) 0)
;    (<= (pos 0) (board :size)) (<= (pos 1) (board :size))))

;(defn valid-pos? [board [row column]]
;  (let [sz (:size board)]
;    (and (> row 0) (> column 0) (<= row sz) (<= column sz))))

;(defmacro valid-pos? [board pos]
;  `(let [{sz# :size} ~board [row# column#] ~pos]
;    (and (> row# 0) (> column# 0) (<= row# sz#) (<= column# sz#))))

(defn valid-pos? [sz [row column]]
    (and (> row 0) (> column 0) (<= row sz) (<= column sz)))

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

