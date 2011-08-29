(ns clojurereversi.stats)

(def empty-stats {'white [] 'black []})

(defn add-duration [color duration data]
  (conj data {color (conj (data color) duration)}))

(defn catdata [data]
  (vec (concat (data 'white) (data 'black))))

(defn average [data]
  (/ (double (apply + data)) (double (count data))))

(defn avg-of-stones [color data] (average (data color)))
(defn avg-of-total [data] (average (catdata data)))

(defn max-of-stones [color data] (apply max (data color)))
(defn max-of-total [data] (apply max (catdata data)))

(defn min-of-stones [color data] (apply min (data color)))
(defn min-of-total [data] (apply min (catdata data)))

(defn duration-total [data] (apply + (catdata data)))

(defn to-msec [nanosecs]
  (/ (double nanosecs) 1000000.0))

(defn to-sec [nanosecs]
  (/ (double nanosecs) 1000000000.0))

(defn to-min [nanosecs]
  (/ (double nanosecs) 1000000000.0 60.0))

(defn frmt [nanosecs]
  (let [secs (to-sec nanosecs)]
    (cond
      (> secs 60.0)
        (format "%.2f mins" (to-min nanosecs))
      (< secs 1.0)
        (format "%.2f msecs" (to-msec nanosecs))
      :else
        (format "%.3f secs" secs))))

(defn print-stats [data]
  (println "\ntotal duration of the game:" (frmt (duration-total data)))
  (println "time per one move:")
  (printf "%10s %20s %20s %20s\n" "" "[white stones]" "[black stones]" "[all stones]")
  (printf "%10s %20s %20s %20s\n" "average:"
    (frmt (avg-of-stones 'white data))
    (frmt (avg-of-stones 'black data))
    (frmt (avg-of-total data)))
  (printf "%10s %20s %20s %20s\n" "max:"
    (frmt (max-of-stones 'white data))
    (frmt (max-of-stones 'black data))
    (frmt (max-of-total data)))
  (printf "%10s %20s %20s %20s\n" "min:"
    (frmt (min-of-stones 'white data))
    (frmt (min-of-stones 'black data))
    (frmt (min-of-total data)))
  (println))

