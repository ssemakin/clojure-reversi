(ns clojurereversi.boardb)

;(defn gen-cell-name [row column]
;  (. clojure.lang.Symbol (intern (str "c" row column))))
(defn gen-cell-name [row column] (symbol (str "c" row column)))

(defrecord Cell [color neighbours])

(defrecord WindRose [n ne e se s sw w nw])
(defrecord SouthEastSector [e se s])
(defrecord SouthSector [e se s sw w])
(defrecord SouthWestSector [s sw w])
(defrecord WestSector [n s sw w nw])
(defrecord NorthWestSector [n w nw])
(defrecord NorthSector [n ne e w nw])
(defrecord NorthEastSector [n ne e])
(defrecord EastSector [n ne e se s])

(defn gen-board-type-cells [size]
  (def nums (range 1 (inc size)))
  (vec (for [r nums c nums] (gen-cell-name r c))))

(defn gen-board-type-args [size]
  (-> (gen-board-type-cells size)
    (conj 'white-stones)
    (conj 'black-stones)
    (conj 'size)))

(defmacro gen-board-type [size]
  (let [name 'Board
        args (gen-board-type-args size)]
    `(defrecord ~name ~args)))

(defn empty-board [size])
