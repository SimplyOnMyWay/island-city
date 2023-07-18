(ns gaclib.vector-adventures
  (:require [quil.core :as q :include-macros true]))

(defn h
  ([] (h 1.0))
  ([value] (* (q/height) value)))

(defn w
  ([] (w 1.0))
  ([value] (* (q/width) value)))

(defn hw
  "maps (w) and (h) to 2d or 3d vector"
  [p]
  (cond
    ;; 2d vector
    (= (count p) 2)
    (let [x (p 0)
          y (p 1)
          p-scaled [(w x) (h y)]]
      p-scaled)
    (= (count p) 3)
    (let [x (p 0)
          y (p 1)
          z (p 2)
          ;; not sure if this is the right scale for z
          p-scaled [(w x) (h y) (w z)]]
      p-scaled)))

(defn vec-partition
  "same as partition but return vector with nested vectors"
  [n v]
  (let [list-partition (partition n v)]
    (into [] (map #(into [] %) list-partition))))

(defn vline
  ([p1 p2]
   (vline p1 p2 [350 100 100 1.0]))
  ([p1 p2 stroke]
   (vline p1 p2 stroke 0.005))
  ([p1 p2 s-col s-weight]
   (let [line-vec (concat (hw p1)  (hw p2))
         line-weight (w s-weight)]
     (q/with-stroke s-col
       (q/stroke-weight line-weight)
       (apply q/line line-vec)))))

(defn vdist
  [p1 p2]
  (apply q/dist (concat p1 p2)))

(defn vadd [& args] 
  "Add two or more vectors together"
  (when (seq args) 
    (apply mapv + args)))

(defn vsub [& args] 
  "Add two or more vectors together"
  (when (seq args) 
    (apply mapv - args)))

(defn subvec-odd
  [v]
  (into [] (map v (range 0 (count v) 2))))

(defn subvec-even
  [v]
  (into [] (into [] (map v (range 1 (count v) 2)))))
