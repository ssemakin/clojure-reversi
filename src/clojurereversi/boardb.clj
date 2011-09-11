(ns clojurereversi.boardb
  (:use [clojurereversi.util]))

(defrecord Cell [color neighbours])

(defrecord FullSector360 [n ne e se s sw w nw])
(defrecord SouthEastSector90 [e se s])
(defrecord SouthSector180 [e se s sw w])
(defrecord SouthWestSector90 [s sw w])
(defrecord WestSector180 [n s sw w nw])
(defrecord NorthWestSector90 [n w nw])
(defrecord NorthSector180 [n ne e w nw])
(defrecord NorthEastSector90 [n ne e])
(defrecord EastSector180 [n ne e se s])

(defn direction-type? [t d]
  (def full360 #{"n" "ne" "e" "se" "s" "sw" "w" "nw"})
  (let [directions (set (keep #(when (val %) (name (key %))) d))
        t-fields (set (map #(:name (bean %)) (seq (:fields (bean t)))))
        t-directions (clojure.set/intersection t-fields full360)]
    (= directions t-directions)))

(defmacro build-sector
  ([d]
    (cond
      (direction-type? FullSector360 d) `(FullSector360. ~(:n d) ~(:ne d) ~(:e d) ~(:se d) ~(:s d) ~(:sw d) ~(:w d) ~(:nw d))
      (direction-type? SouthEastSector90 d) `(SouthEastSector90. ~(:e d) ~(:se d) ~(:s d))
      (direction-type? SouthSector180 d) `(SouthSector180. ~(:e d) ~(:se d) ~(:s d) ~(:sw d) ~(:w d))
      (direction-type? SouthWestSector90 d) `(SouthWestSector90. ~(:s d) ~(:sw d) ~(:w d))
      (direction-type? WestSector180 d) `(WestSector180. ~(:n d) ~(:s d) ~(:sw d) ~(:w d) ~(:nw d))
      (direction-type? NorthWestSector90 d) `(NorthWestSector90. ~(:n d) ~(:w d) ~(:nw d))
      (direction-type? NorthSector180 d) `(NorthSector180. (:n d) ~(:ne d) ~(:e d) ~(:w d) ~(:nw d))
      (direction-type? NorthEastSector90 d) `(NorthEastSector90. ~(:n d) ~(:ne d) ~(:e d))
      (direction-type? EastSector180 d) `(EastSector180. ~(:n d) ~(:ne d) ~(:e d) ~(:se d) ~(:s d))
      :else (throw (new Exception (str "Unknown direction type!! " d))))))

(def dirs {:n north-dir :ne north-east-dir :e east-dir :se south-east-dir
           :s south-dir :sw south-west-dir :w west-dir :nw north-west-dir})

(defn find-neighbours [size [r c]]
  (reduce merge (keep #(when (valid-pos? size ((val %) [r c])) {(key %) ((val %) [r c])}) dirs)))

(defn gen-cell-name [row column] (symbol (str "c" row column)))

(defn gen-board-type-cells [size]
  (def nums (range 1 (inc size)))
  (vec (for [r nums c nums] (gen-cell-name r c))))

(defmacro init-board-cells [size]
  (def nums (range 1 (inc size)))
  (vec (for [r nums c nums]
         `(Cell. nil ~(macroexpand-1 `(build-sector ~(find-neighbours size [r c])))))))

(defn gen-board-type-args [size]
  (-> (gen-board-type-cells size)
    (conj 'white-stones) (conj 'black-stones) (conj 'size)))

(defmacro init-board-args [size]
  (-> (macroexpand-1 `(init-board-cells ~size))
    (conj #{}) (conj #{}) (conj size)))

(defmacro gen-board-type [size]
  (let [name 'Board
        args (gen-board-type-args size)]
    `(defrecord ~name ~args)))

(defn empty-board [size]
  (def board-type (eval (macroexpand-1 `(gen-board-type ~size))))
  (def board-args (macroexpand-1 `(init-board-args ~size)))
  (eval (macroexpand-1 `(new ~board-type ~@board-args))))

;(defn change-color [board [r c] color]
;  (assoc board (gen-cell-name r c)
;    (Cell. color )))
