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

(defn init-neighbours
  ([{n :n ne :ne e :e se :se s :s sw :sw w :w nw :nw}]
    (cond
      (and n ne e se s sw w nw) (println "all are in plce")
      (and e se s (not (or n ne sw w nw))) (println "detected: e se s")
      :else (println "something is wrong")
      )
    )
  )

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

(defmacro new-obj [o-type & ctr-values]
  `(new ~(eval o-type) ~@ctr-values))

(defn empty-board [size])

