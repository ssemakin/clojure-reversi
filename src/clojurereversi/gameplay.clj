(ns clojurereversi.gameplay
  (:use [clojurereversi.core
         :only (valid-pos? empty-cell? cell-color? mput-color put-stone rival-color positions directions)]))

(defn end-of-game? [moves rival-moves]
  (and (empty? moves) (empty? rival-moves)))

(defn find-state-in-dir [direction needed-state? start-pos capture-color board]
  (loop [s start-pos, captured {}]
    (let [next-pos (direction s)]
      (cond
        (= (board next-pos) capture-color)
          (recur next-pos (conj captured {:captured (concat (captured :captured) [next-pos])}))
        (and (needed-state? board next-pos) (= (board s) capture-color) (valid-pos? board next-pos))
          (conj captured {:covering-pos next-pos})
        :else {}))))

(defn find-move-in-dir [direction color start-pos board]
  (find-state-in-dir direction empty-cell? start-pos (rival-color color) board))

(defn find-moves [color board]
  (loop [poss (positions color board) moves #{}]
    (if-let [pos (first poss)]
      (let [xs (->>
                  (map #((find-move-in-dir % color pos board) :covering-pos) directions)
                  (set)
                  (filter #(not-empty %)))]
        (recur (rest poss) (set (concat moves xs))))
      moves)))

(defn find-covering-stone-in-dir [direction color start-pos board]
    (find-state-in-dir direction #(cell-color? color %1 %2) start-pos (rival-color color) board))

(defn do-adjacent-for-stone [color pos board]
  (defn do-adjacent [color positions board]
    (if (first positions)
      (recur color (rest positions) (do-adjacent-for-stone color (first positions) board))
      board))

  (loop [dirs directions, result-board board]
    (if (first dirs)
      (let [nfound (find-covering-stone-in-dir (first dirs) color pos result-board)
            board-with-flipped (mput-color color result-board (nfound :captured))
            processed-board (do-adjacent color (nfound :captured) board-with-flipped)]
        (recur (rest dirs) processed-board))
      result-board)))

(defn play-color [color pos board]
  (do-adjacent-for-stone color pos (put-stone color board pos)))
