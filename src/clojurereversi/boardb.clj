(ns clojurereversi.boardb)

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

(defn empty-board [size])

