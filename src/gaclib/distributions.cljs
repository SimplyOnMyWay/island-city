(ns gaclib.distributions
  (:require [quil.core :as q]
            [kixi.stats.distribution :refer [binomial uniform pareto draw sample]]))

(defmulti gauss
  ""
  (fn [args] (first args)))

(defmethod gauss "a"
  [gauss-variant mean sd]
  (+ mean (* sd (q/random-gaussian))))

(defmethod gauss "b"
  ([gauss-variant mean sd]
   (gauss "b" mean sd 3))
  ([gauss-variant mean sd number-of-sd]
   (let [value-pre (+ mean (* sd (q/random-gaussian)))
         sd  (.pow js/Math sd 1) ;variance = sd**2
         min (- mean (* number-of-sd sd)) ; 99.7% data following a gaussian distribution falls within 3 standard deviations. 
         max (+ mean (* number-of-sd sd))
         value (cond (< value-pre min) mean
                     (> value-pre max) mean
                     :else value-pre)]
     value)))

(defmethod gauss "c"
  ([gauss-variant mean sd limit]
   (gauss "c" mean sd 3 limit))
  ([gauss-variant mean sd number-of-sd limit]
   (let [value-pre (+ mean (* sd (q/random-gaussian)))
         sd  (.pow js/Math 1) ;variance = sd**2
         min limit;(- mean (* number-of-sd sd)) ; 99.7% data following a gaussian distribution falls within 3 standard deviations. 
         max (- 1.0 limit)   ;(+ mean (* number-of-sd sd))
         value (cond (< value-pre min) mean
                     (> value-pre max) mean
                     :else value-pre)]
     value)))

(defn pareto-samples
  ([num-samples scale]
   (pareto-samples num-samples scale nil 1.16))
  ([num-samples scale seed]
   (pareto-samples num-samples scale seed 1.16))
  ([num-samples scale seed shape]
   (if seed
     (sample num-samples (pareto {:scale scale :shape shape}){:seed seed})
     (sample num-samples (pareto {:scale scale :shape shape})))))

(defn pareto-samples-max
  ([num-samples scale max]
   (pareto-samples-max num-samples scale nil 1.16 max))
  ([num-samples scale seed max]
   (pareto-samples-max num-samples scale seed 1.16 max))
  ([num-samples scale seed shape max]
   (if seed
     (let [val (first (sample num-samples (pareto {:scale scale :shape shape}){:seed seed}))]
       (if (< val max)
         val
         max
         ;scale
         ))
     (let [val (first (sample num-samples (pareto {:scale scale :shape shape})))]
       (if (< val max)
         val
         max
         ;scale
         )))))
