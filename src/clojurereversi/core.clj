(ns clojurereversi.core)

;(defn north-dir [pos] [(dec (pos 0)) (pos 1)])
;(defn north-east-dir [pos] [(dec (pos 0)) (inc (pos 1))])
;(defn east-dir [pos] [(pos 0) (inc (pos 1))])
;(defn south-east-dir [pos] [(inc (pos 0)) (inc (pos 1))])
;(defn south-dir [pos] [(inc (pos 0)) (pos 1)])
;(defn south-west-dir [pos] [(inc (pos 0)) (dec (pos 1))])
;(defn west-dir [pos] [(pos 0) (dec (pos 1))])
;(defn north-west-dir [pos] [(dec (pos 0)) (dec (pos 1))])

;(defn north-dir [pos] [(dec (first pos)) (pos 1)])
;(defn north-east-dir [pos] [(dec (first pos)) (inc (pos 1))])
;(defn east-dir [pos] [(first pos) (inc (pos 1))])
;(defn south-east-dir [pos] [(inc (first pos)) (inc (pos 1))])
;(defn south-dir [pos] [(inc (first pos)) (pos 1)])
;(defn south-west-dir [pos] [(inc (first pos)) (dec (pos 1))])
;(defn west-dir [pos] [(first pos) (dec (pos 1))])
;(defn north-west-dir [pos] [(dec (first pos)) (dec (pos 1))])


(defn north-dir [[r c]] [(dec r) c])
(defn north-east-dir [[r c]] [(dec r) (inc c)])
(defn east-dir [[r c]] [r (inc c)])
(defn south-east-dir [[r c]] [(inc r) (inc c)])
(defn south-dir [[r c]] [(inc r) c])
(defn south-west-dir [[r c]] [(inc r) (dec c)])
(defn west-dir [[r c]] [r (dec c)])
(defn north-west-dir [[r c]] [(dec r) (dec c)])


(def directions [north-dir north-east-dir east-dir south-east-dir
                 south-dir south-west-dir west-dir north-west-dir])

(defn pos-middle-up-left [size] [(/ size 2) (/ size 2)])
(defn pos-middle-up-right [size] [(/ size 2) (+ (/ size 2) 1)])
(defn pos-middle-down-left [size] [(+ (/ size 2) 1) (/ size 2)])
(defn pos-middle-down-right [size] [(+ (/ size 2) 1) (+ (/ size 2) 1)])

;(defn black? [board pos] (= (board pos) :black))
;(defn white? [board pos] (= (board pos) :white))
;(defn empty-cell? [board pos] (nil? (board pos)))
;(defn cell-color? [color board pos] (= (board pos) color))

(defmacro black? [board pos] `(= ~@(board pos) :black))
(defmacro white? [board pos] `(= ~@(board pos) :white))
(defn empty-cell? [board pos] (nil? (board pos)))
(defn cell-color? [color board pos] (= (board pos) color))

(defn put-black [board position] (conj board {position :black}))
(defn put-white [board position] (conj board {position :white}))
;(defn put-stone [color board position] (conj board {position color}))
(defmacro put-stone [color board position] `(conj ~board {~position ~color}))
(defn mput-color [color board positions]
  (if-let [pos (first positions)]
    (recur color (put-stone color board pos) (rest positions))
    board))

(def rival-color {:black :white, :white :black})

;(defn positions [color board]
;  (keep #(when (= (% 1) color) (% 0)) board))

;(defn positions [color board]
;  (keep (fn [[pos stone]] (when (= stone color) pos)) board))

(defmacro positions [color board]
  `(keep (fn [[pos# stone#]] (when (= stone# ~color) pos#)) ~board))


;(defn valid-pos? [board pos]
;  (and (> (pos 0) 0) (> (pos 1) 0)
;    (<= (pos 0) (board :size)) (<= (pos 1) (board :size))))

;(defn valid-pos? [board [row column]]
;  (let [sz (:size board)]
;    (and (> row 0) (> column 0) (<= row sz) (<= column sz))))

;(defn valid-pos? [{sz :size} [row column]]
;    (and (> row 0) (> column 0) (<= row sz) (<= column sz)))

(defmacro valid-pos? [board pos]
  `(let [{sz# :size} ~board [row# column#] ~pos]
    (and (> row# 0) (> column# 0) (<= row# sz#) (<= column# sz#))))

(defn empty-board [size] (hash-map :size size))

(defn init-board [size]
  (-> (empty-board size)
    (put-white (pos-middle-up-left size))
    (put-black (pos-middle-up-right size))
    (put-black (pos-middle-down-left size))
    (put-white (pos-middle-down-right size)))
  )
