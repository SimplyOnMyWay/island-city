(ns gaclib.border-art
  (:require [quil.core :as q :include-macros true]))

(defn h
  ([] (h 1.0))
  ([value] (* (q/height) value)))

(defn w
  ([] (w 1.0))
  ([value] (* (q/width) value)))

(defn dots
  ([dot-gap dot-weight dot-border dot-colour]
   (q/with-stroke dot-colour
     (q/stroke-weight (w dot-weight))
     (doseq [x (range dot-border (- 1.0 (- dot-border dot-gap)) dot-gap)]
       (q/point (w x) (h dot-border)))
     (doseq [y (range dot-border (- 1.0 (- dot-border dot-gap)) dot-gap)]
       (q/point (w dot-border) (h y)))
     (doseq [x (range dot-border (- 1.0 (- dot-border dot-gap)) dot-gap)]
       (q/point (w x) (h (- 1.0 dot-border))))
     (doseq [y (range dot-border (- 1.0 (- dot-border dot-gap)) dot-gap)]
       (q/point (w (- 1.0 dot-border)) (h y)))))
  ([dot-gap]
   (dots dot-gap 0.01 0.03 [220 70 70 0.7]))
  ([]
   (dots 0.02)))
