(ns gaclib.line-ops
  (:require [quil.core :as q]
            [gaclib.vector-adventures :as v]
            [gaclib.gui :refer [w h hw]]
            [gaclib.distributions :as d]
            [thi.ng.geom.vector :as tv]
            [thi.ng.geom.core :as tg]
            [thi.ng.math.core :as tm]
            [thi.ng.geom.line :as tl]
            [thi.ng.geom.utils.intersect :as isec]
            [thi.ng.geom.utils :as tu]))

(defn split-line
  "generalises line to a thing with start and end and any number of positive and negative space straight sections in between.
  works fine for either 2d or 3d position and gap-vec vectors, as long as BOTH consistent of course!"
  ([p-s p-e gap-vec]
   (split-line p-s p-e gap-vec [350 100 100 1.0] 0.007))
  ([p-s p-e gap-vec stroke]
   (split-line p-s p-e gap-vec stroke 0.007))
  ([p-s p-e gap-vec stroke stroke-weight]
   (let [ ;; convert gaps to line sections and bookend with p-s and p-e
         line-sections (v/vec-partition 2 (reduce into [[p-s] gap-vec [p-e]]))]
     (q/with-stroke stroke
       (q/stroke-weight (w stroke-weight))
       (doseq [[s e] line-sections]
         (v/vline s e stroke stroke-weight))))))

(defn gaps-from-t
  [s e t-vec]
  (let [s (tv/vec3 s)
        e (tv/vec3 e)
        dr (tm/- e s)
        dr-norm (tm/normalize dr)
        gap-vec (mapv #(tm/+ s (tg/scale dr-norm %)) t-vec)]
    gap-vec))

(defn generate-margins 
  [i-vec margin-vec]
  [(tm/- i-vec margin-vec) (tm/+ i-vec margin-vec)])

(defn gaps-from-i
  "note i-vec is a vector of vectors"
  [s e i-vec margin]
  (let [dr (tm/- e s)
        dr-norm (tm/normalize dr)
        dr-margin (tg/scale dr-norm margin)
        [gap-vec] (mapv #(generate-margins % dr-margin) i-vec)]
    gap-vec))

(defn generate-road-from-centerlines
  "superceded by gen-road-lines - keeping for a while aenh (ar eagla na heagla)"
  [l road-width pavement-width]
 (let [[s e] (tg/vertices l)
       dr (tm/- e s)
       dr-norm (tm/normalize dr)
       dr-perp (tm/cross dr-norm tv/V3Z)
       t-u' (/ road-width 2) ;; dist middle to edge of road (rel to dr-norm, scales with (w)(h) and maybe depth also
       t-u'' (+ pavement-width t-u') ;; dist middle of road to edge of path
       rw-vec (tg/scale dr-perp t-u')
       pw-vec (tg/scale dr-perp t-u'')
       rw-vec-inv (map #(* -1 %) rw-vec)
       pw-vec-inv (map #(* -1 %) pw-vec)
       l'+ (tg/translate l rw-vec)
       l''+ (tg/translate l pw-vec)
       l'- (tg/translate l rw-vec-inv)
       l''- (tg/translate l pw-vec-inv)
       ]
   [l'+ l''+ l'- l''-]))

(defmulti paved-road
  "paved road with middle of the road defined by line connecting s(x,y) to e(x,y), and gaps defined either by
:i gaps-from-i
or
:t gaps-from-t"
  (fn [args] (first args)))

(defmethod paved-road "i"
  ([gap-gen-method s e road-width pavement-width]
   (paved-road gap-gen-method s e road-width pavement-width [] 0 0 0.007 0.007))
  ([gap-gen-method s e road-width pavement-width gap-vec road-colour pavement-colour road-stroke-weight pavement-stroke-weight]
   (let [s (tv/vec3 s) ;; in case not entered as th.ingvec3
         e (tv/vec3 e)
         l (tl/line3 s e)
         l-norm (tm/normalize l)
         dr (tm/- e s)
         dr-norm (tm/normalize dr)
         dr-perp (tm/cross dr-norm tv/V3Z)
         t-u' (/ road-width 2) ;; dist middle to edge of road (rel to dr-norm, scales with (w)(h) and maybe depth also
         t-u'' (+ pavement-width t-u') ;; dist middle of road to edge of path
         s'+ (tm/+ s (tg/scale dr-perp t-u'))
         s''+ (tm/+ s (tg/scale dr-perp t-u''))
         s'- (tm/- s (tg/scale dr-perp t-u'))
         s''- (tm/- s (tg/scale dr-perp t-u''))
         t-v 1.0 ;; longitudinal distance ratio (rel to dr)
         e'+ (tm/+ s'+ (tg/scale dr t-v))
         e''+ (tm/+ s''+ (tg/scale dr t-v))
         e'- (tm/+ s'- (tg/scale dr t-v))
         e''- (tm/+ s''- (tg/scale dr t-v))
         l'+ (tl/line3 s'+ e'+)
         l''+ (tl/line3 s''+ e''+)
         l'- (tl/line3 s'- e'-)
         l''- (tl/line3 s''- e''-)
         ;i'+ ()
         gap-vec'+ (gap-vec 1)                ;(gaps-from-i s'+ e'+ )
         gap-vec''+ (gap-vec 0)
         gap-vec'- (gap-vec 2)
         gap-vec''- (gap-vec 3)           ;(gaps-from-t s''- e''- gap-vec)
         ]
     (split-line s''+ e''+ gap-vec''+ road-colour road-stroke-weight)
     (split-line s'+ e'+ gap-vec'+ pavement-colour pavement-stroke-weight)
     (split-line s'- e'- gap-vec'- pavement-colour pavement-stroke-weight)
     (split-line s''- e''- gap-vec''- road-colour road-stroke-weight))))

(defmethod paved-road "t"
  ([gap-gen-method s e road-width pavement-width]
   (paved-road gap-gen-method s e road-width pavement-width [] 0 0 0.007))
  ([gap-gen-method s e road-width pavement-width gap-vec road-colour pavement-colour road-stroke-weight pavement-stroke-weight]
   (let [s (tv/vec3 s) ;; in case not entered as th.ingvec3
         e (tv/vec3 e)
         l (tl/line3 s e)
         l-norm (tm/normalize l)
         dr (tm/- e s)
         dr-norm (tm/normalize dr)
         dr-perp (tm/cross dr-norm tv/V3Z)
         t-u' (/ road-width 2) ;; dist middle to edge of road (rel to dr-norm, scales with (w)(h) and maybe depth also
         t-u'' (+ pavement-width t-u') ;; dist middle of road to edge of path
         s'+ (tm/+ s (tg/scale dr-perp t-u'))
         s''+ (tm/+ s (tg/scale dr-perp t-u''))
         s'- (tm/- s (tg/scale dr-perp t-u'))
         s''- (tm/- s (tg/scale dr-perp t-u''))
         t-v 1.0 ;; longitudinal distance ratio (rel to dr)
         e'+ (tm/+ s'+ (tg/scale dr t-v))
         e''+ (tm/+ s''+ (tg/scale dr t-v))
         e'- (tm/+ s'- (tg/scale dr t-v))
         e''- (tm/+ s''- (tg/scale dr t-v))
         gap-vec'+ (gaps-from-t s'+ e'+ gap-vec)
         gap-vec''+ (gaps-from-t s''+ e''+ gap-vec)
         gap-vec'- (gaps-from-t s'- e'- gap-vec)
         gap-vec''- (gaps-from-t s''- e''- gap-vec)]
     (split-line s''+ e''+ gap-vec''+ road-colour road-stroke-weight)
     (split-line s'+ e'+ gap-vec'+ pavement-colour pavement-stroke-weight)
     (split-line s'- e'- gap-vec'- pavement-colour pavement-stroke-weight)
     (split-line s''- e''- gap-vec''- road-colour road-stroke-weight))))

(defn draw-paved-road
  [r]
  (paved-road "i" (:s r) (:e r) (:rw r) (:pw r) (:intersections r) (:rc r) (:pc r) (:rs r) (:ps r)))

(defn gen-road-lines
  [r]
  (let [s (:s r)
        e (:e r)
        l (tl/line3 s e)
        rw (:rw r)
        pw (:pw r)
        dr (tm/- e s)
        dr-norm (tm/normalize dr)
        dr-perp (tm/cross dr-norm tv/V3Z)
        t-u' (/ rw 2) ;; dist middle to edge of road (rel to dr-norm, scales with (w)(h) and maybe depth also
        t-u'' (+ pw t-u') ;; dist middle of road to edge of path
        rw-vec (tg/scale dr-perp t-u')
        pw-vec (tg/scale dr-perp t-u'')
        rw-vec-inv (map #(* -1 %) rw-vec)
        pw-vec-inv (map #(* -1 %) pw-vec)
        l'+ (tg/translate l rw-vec)
        l''+ (tg/translate l pw-vec)
        l'- (tg/translate l rw-vec-inv)
        l''- (tg/translate l pw-vec-inv)]
    [l'+ l''+ l'- l''-]))

(defn draw-road-lines
  "this is to check the function gen-road-lines aligns works! Especially that it does the same as the function paved-road only without the gaps"
  [road-lines]
  (let [road-lines-as-vecs (map :points road-lines)]
    (doseq [l road-lines-as-vecs]
      (apply v/vline l))))

(defn gen-centerline
  [r]
  (tl/line3 (:s r) (:e r)))

(defn draw-centerline
  [r]
  (v/vline (:s r) (:e r) 150 0.001))

(defn gen-road-combos-repeated
  "use together with gen-road-combos-interleaved"
  [r]
  (let [r-lines (gen-road-lines r)]
    (into [] (concat r-lines r-lines r-lines r-lines))))

(defn gen-road-combos-interleaved
  "generates vector of road lines arranged in an 'interleaved' order, which when paired with the function gen-road-combos-repeated gives by 'brute force' all the permutations lines in two roads cross each other. These two functions used together like this works, though there is probably a more elegant way using reduce"
  [r]
  (let [r-lines (gen-road-lines r)]
    (into [] (concat (repeat 4 (r-lines 0)) (repeat 4 (r-lines 1)) (repeat 4 (r-lines 2)) (repeat 4 (r-lines 3))))))

(defn intersect-line-2D
;; important helper function allowing 2D intersections be found for lines defined by 3D points.  Specfies interestions which are outside the line via the intersect-outside key
  [{[a b] :points} l]
    (let [[c d] (get l :points l)]
      (isec/intersect-line2-line2? a b c d))) 

(defn gen-road-intersections
  "identifies 2D coords of intersection points by mapping intersect-line across sequences of the combinations in which road lines cross each other at an intersection. 

Important: uses :type key returned by intersect-line-2D to filter out :type of :intersect-outside"
  ([r1 r2]
   (gen-road-intersections r1 r2 nil))
  ([r1 r2 verbose?]
   (let [r1-combos (gen-road-combos-repeated r1)
         r2-combos (gen-road-combos-interleaved r2)
         i12 (into [] (map :p (filter #(= (:type %) :intersect) (map #(intersect-line-2D %1 %2) r1-combos r2-combos))))]
     (if (< (count i12) 16)
       (if (= verbose? :v)
         (println (str "filtered i12 out as fewer than 16 points within intersection.  Road centrelines are probably overlapping by less than road-width and/or (+ road-width path-width)")))
       i12))))

(defn gen-road-intersections-precise
  [r1 r2]
  (let [i12 (gen-road-intersections r1 r2)
        i12-outer-outer (into [] (map #(get i12 %) [5 7 13 15]))
        i12-outer-inner (into [] (map #(get i12 %) [1 3 4 6 9 11 12 14]))
        i12-inner-inner (into [] (map #(get i12 %) [0 2  8 10]))]
    {:outer-outer i12-outer-outer
     :outer-inner i12-outer-inner
     :inner-inner i12-inner-inner}))

(defn gen-road-intersections-ordered
  "just reorders the vector to the following conventions:
index 0 - 3 outer corners starting at s end on left
index 4 - 7 inner tramline intersects with outer, starting at s end on left, then s end on right, then e end on left...
index 8 - 11 outer tramline intersects with inner perp
index 12 - 15 inner intersects s left/right e left/right"
  [r1 r2]
  (let [i12 (gen-road-intersections r1 r2)
        i-empty (into [] (repeat 16 []))
        i12-ordered (if (empty? i12)
                      i-empty
                      (into [] (map #(i12 %) [5 7 13 15 4 6 12 14 1 3 9 11 0 2 8 10])))]
    i12-ordered))

(defn draw-road-intersections
  [r1 r2]
  (let [i12 (gen-road-intersections r1 r2)]
    (doseq [i i12]
      (q/with-translation [0 0 (w 0.001)]
        (q/with-stroke [50 100 100 0.1]
          (q/stroke-weight (w 0.01))
          (apply q/point (hw i)))))))

(defn draw-road-intersections-precise
  "not really used - legacy first effort, replaced by draw-road-intersections-ordered"
  [r1 r2]
  (let [i12 (gen-road-intersections-precise r1 r2)
        i-colours {:outer-outer [0 100 100 1.0]
                   :outer-inner [120 100 100 1.0]
                   :inner-inner [240 100 100 1.0]}]
    (q/with-translation [0 0 (w 0.001)]
      (q/stroke-weight (w 0.01))
      (doseq [i (range (count (i12 :outer-outer)))]
        (q/with-stroke (:outer-outer i-colours)
          (apply q/point (hw ((:outer-outer i12) i)))))
      (doseq [i (range (count (i12 :outer-inner)))]
        (q/with-stroke (:outer-inner i-colours)
          (apply q/point (hw ((:outer-inner i12) i)))))
      (doseq [i (range (count (i12 :inner-inner)))]
        (q/with-stroke (:inner-inner i-colours)
          (apply q/point (hw ((:inner-inner i12) i))))))))

(defn draw-road-intersections-ordered
  ([r1 r2 s]
   (let [i12 (gen-road-intersections-ordered r1 r2)
         i-colours  [[0 100 100 1.0]
                     [120 100 100 1.0]
                     [180 30 50 1.0]
                     [240 100 100 1.0]]]
     (q/with-translation [0 0 (w 0.001)]
       (q/stroke-weight (w s))
       (doseq [i [0 1 2 3]]
         (q/with-stroke (i-colours 0)
           (apply q/point (hw (i12 i)))))
       (doseq [i [4 5 6 7]]
         (q/with-stroke (i-colours 1)
           (apply q/point (hw (i12 i)))))
       (doseq [i [8 9 10 11]]
         (q/with-stroke (i-colours 2)
           (apply q/point (hw (i12 i)))))
       (doseq [i [12 13 14 15]]
         (q/with-stroke (i-colours 3)
           (apply q/point (hw (i12 i))))))))
  ([r1 r2]
   (draw-road-intersections-ordered r1 r2 0.005)))

;; helper functions for creating gap-vec
(defn left-outer-gap
  [i]
  (if (or (and (= (i 0) []) (= (i 2) []))
          (< (count i) 16))
    []
    [(i 0) (i 2)]))

(defn left-inner-gap
  [i]
  (if (or (and (= (i 4) []) (= (i 6) []))
          (< (count i) 16))
    []
    [(i 4) (i 6)]))

(defn right-inner-gap
  [i]
  (if (or (and (= (i 5) []) (= (i 7) []))
          (< (count i) 16))
    []
    [(i 5) (i 7)]))

(defn right-outer-gap
  [i]
  (if (or (and (= (i 1) []) (= (i 3) []))
          (< (count i) 16))
    []
    [(i 1) (i 3)]))

(defn gen-gap-vec
  "inputs intersection generated by gen-road-intersections-ordered and outputs a gap-vec to be passed to paved-road (either as key value for road, or as additional arg to draw-paved-road"
  [i-vec]
  (cond (> (count i-vec) 1)
        (let [left-outer-gaps (v/vec-partition 3 (flatten (map #(left-outer-gap %) i-vec)))
              left-inner-gaps (v/vec-partition 3 (flatten (map #(left-inner-gap %) i-vec)))
              right-inner-gaps (v/vec-partition 3 (flatten (map #(right-inner-gap %) i-vec)))
              right-outer-gaps (v/vec-partition 3 (flatten (map #(right-outer-gap %) i-vec)))
              gap-vec [left-outer-gaps
                       left-inner-gaps
                       right-inner-gaps
                       right-outer-gaps]]
          gap-vec)
        :else (let [[i] i-vec
                    gap-vec [(left-outer-gap i)
                             (left-inner-gap i)
                             (right-inner-gap i)
                             (right-outer-gap i)]]
                gap-vec)))

(defn gen-gap-vec-reverse
  "same as gen-gap-vec only for when left tramline of first road meets right tramline of second road inially"
  [i-vec]
  (cond (> (count i-vec) 1)
        (let [left-outer-gaps (v/vec-partition 3 (flatten (map #(reverse (left-outer-gap %)) i-vec)))
              left-inner-gaps (v/vec-partition 3 (flatten (map #(reverse (left-inner-gap %)) i-vec)))
              right-inner-gaps (v/vec-partition 3 (flatten (map #(reverse (right-inner-gap %)) i-vec)))
              right-outer-gaps (v/vec-partition 3 (flatten (map #(reverse (right-outer-gap %)) i-vec)))
              gap-vec [left-outer-gaps
                       left-inner-gaps
                       right-inner-gaps
                       right-outer-gaps]]
          gap-vec)
        :else (let [[i] i-vec
                    gap-vec [(reverse (left-outer-gap i))
                             (reverse (left-inner-gap i))
                             (reverse (right-inner-gap i))
                             (reverse (right-outer-gap i))]]
                gap-vec)))

(defn gen-road
  "s:    starting point
       e:    end point
       rw:   road width
       pw:   path width
       rc:   road-colour
       pc:   path-colour
       rs:   road-stroke-weight
       ps:   path-stroke-weight
       intersections: vector of intersections to be filled in later via helper functions - redundant probably delete later"
  ([s e rw pw rc pc rs ps]
   (let [gap-vec-empty [[] [] [] []]]
     {:s (tv/vec3 s)
      :e (tv/vec3 e)
      :rw rw
      :pw pw
      :pc pc
      :rc rc
      :rs rs
      :ps ps
      :intersections gap-vec-empty}))
  ([s e rw pw rc pc]
   (gen-road s e rw pw rc pc 0.005 0.005))
  ([s e rw pw]
   (gen-road s e rw pw [150 40 90 1.0] [340 70 80 1.0]))
  ([s e]
   (gen-road s e 0.02 0.01)))


(defn gappify-all-roads
  [east-roads north-roads]
  (let [no-east (count east-roads)
        no-north (count north-roads)
        east-combos (into [] (flatten (map #(repeat no-north %) east-roads)))
        north-combos (into [] (flatten (repeat no-east north-roads)))
        i-all-roads (into [] (map  #(gen-road-intersections-ordered %1 %2) east-combos north-combos))
        ]
    i-all-roads))

(defn gappify-all-roads-reverse
  [east-roads north-roads]
  (let [no-east (count east-roads)
        no-north (count north-roads)
        east-combos (into [] (flatten (repeat no-north east-roads)))
        north-combos (into [] (flatten (map #(repeat no-east %) north-roads)))
        i-all-roads (into [] (map  #(gen-road-intersections-ordered %1 %2) north-combos east-combos))
        ]
    i-all-roads))

(defn north-gap-vec-reverse
  "needed to correct some funkiness when drawing gaps properly for north roads. The gaps created by gen-gap-vec are in exactly the reverse order, so this function works better in this situation than gen-gap-vec-reverse. May supercede it even though less intuitive"
  [gap-vec]
  (let [[v1 & vrest] gap-vec                                                                                     
        v1-rev (into [] (reverse v1))                                                                                 
        vrest-rev (into []  (map #(into [] (reverse %)) vrest))]                                                      
    (into  [v1-rev] vrest-rev)))

(defn draw-roads-gapped
  "this is the biggie! enough said :)"
  [east-roads north-roads]
  (let [i-e-n (gappify-all-roads east-roads north-roads)
        i-n-e (gappify-all-roads-reverse east-roads north-roads)
        num-e (count east-roads)
        num-n (count north-roads)
        east-gaps (mapv #(gen-gap-vec (subvec i-e-n (* % num-n) (+ (* %  num-n) num-n))) (range num-e))
        north-gaps (mapv #(north-gap-vec-reverse (gen-gap-vec (subvec i-n-e (* % num-e) (+ (* % num-e) num-e)))) (range num-n))
        east-roads-gapped (mapv #(assoc %1 :intersections %2) east-roads east-gaps)
        north-roads-gapped (mapv #(assoc %1 :intersections %2) north-roads north-gaps)]
    (doseq [e-num (range (count east-roads))]
      (draw-paved-road (east-roads-gapped e-num)))
    (doseq [n-num (range (count north-roads))]
      (draw-paved-road (north-roads-gapped n-num)))))

;; generate s-east e-east s-north e-north, ie a sort of outer edge for the roads
(defn gen-edges-diag
  ([]
   (gen-edges-diag 0.05 0.1 0.9 0.1 0.9))
  ([step]
   (gen-edges-diag step 0.1 0.9))
  ([step s e]
   (gen-edges-diag step s e s e))
  ([step xs xe ys ye]
   (let [p1 (tv/vec3 [xs ys 0])
         p2 (tv/vec3 [xs ye 0])
         p3 (tv/vec3 [xe ys 0])
         p4 (tv/vec3 [xe ye 0])
         r (into [] (range 0 1.0 step))
         s-east (mapv #(tu/point-at-index [p1 p2 p4] %) r)
         e-east (mapv #(tu/point-at-index [p1 p3 p4] %) r)
         s-north (mapv #(tu/point-at-index [p2 p4 p3] %) r)
         e-north (mapv #(tu/point-at-index [p2 p1 p3] %) r)]
     {:s-east s-east :e-east e-east :s-north s-north :e-north e-north})))


(defn draw-edges-diag
  "visualise gen-edges-diag
step xs xe ys ye as per gen-edges-diag
c = colour
sw = stroke-weight"
  ([edges]
   (draw-edges-diag edges [50 80 80 1.0] 0.002))
  ([edges c sw]
   (let [se (:s-east edges)
         ee (:e-east edges)
         sn (:s-north edges)
         en (:e-north edges)]
     (doseq [i (range 0 (count se))]
       (v/vline (se i) (ee i) c sw))
     (doseq [i (range 0 (count sn))]
       (v/vline (sn i) (en i) c sw)))))



(defn gauss-2d
  ([vec2 x-sd y-sd]
   (gauss-2d vec2 x-sd y-sd 0.0 0.0))
  ([vec2 x-sd y-sd edge x-offset]
   (let [x-mean (vec2 0)
         y-mean (vec2 1)
         x- (d/gauss "b" x-mean x-sd)
         y- (d/gauss "b" y-mean y-sd)
         x-min (+ edge x-offset)
         x-max (- 1.0 (- edge x-offset))
         y-min edge
         y-max (- 1.0 edge)
         x (cond (< x- x-min) x-min
                 (> x- x-max) x-max
                 :else x-)
         y (cond (< y- y-min) y-min
                 (> y- y-max) y-max
                 :else y-)]
     (tv/vec2 x y))))

(defn gauss-3d
  [vec3 x-sd y-sd z-sd edge x-offset]
  (let [x-mean (vec3 0)
        y-mean (vec3 1)
        z-mean (vec3 2)
        x- (d/gauss "b" x-mean x-sd)
        y- (d/gauss "b" y-mean y-sd)
        z- (d/gauss "b" z-mean z-sd)
        x-min (+ edge x-offset)
        x-max (- 1.0 (- edge x-offset))
        y-min edge
        y-max (- 1.0 edge)
        x (cond (< x- x-min) x-min
                (> x- x-max) x-max
                :else x-)
        y (cond (< y- y-min) y-min
                (> y- y-max) y-max
                :else y-)
        z z-]
    (tv/vec3 x y z)))

(defn gen-edges-diag-gauss
  ([]
   (gen-edges-diag 0.05 0.1 0.9 0.1 0.9))
  ([step]
   (gen-edges-diag step 0.1 0.9))
  ([step s e]
   (gen-edges-diag step s e s e))
  ([step xs xe ys ye x-sd y-sd edge x-offset]
   (let [p1 (tv/vec3 [xs ys 0])
         p2 (tv/vec3 [xs ye 0])
         p3 (tv/vec3 [xe ys 0])
         p4 (tv/vec3 [xe ye 0])
         r (into [] (range 0 1.0 step))
         s-east (mapv #(gauss-3d (tu/point-at-index [p1 p2 p4] %) x-sd y-sd 0 edge x-offset) r)
         e-east (mapv #(gauss-3d (tu/point-at-index [p1 p3 p4] %) x-sd y-sd 0 edge x-offset) r)
         s-north (mapv #(gauss-3d (tu/point-at-index [p2 p4 p3] %)x-sd y-sd 0 edge x-offset) r)
         e-north (mapv #(gauss-3d (tu/point-at-index [p2 p1 p3] %)x-sd y-sd 0 edge x-offset) r)]
     {:s-east s-east :e-east e-east :s-north s-north :e-north e-north})))

(defn gen-edges-horizontal-vertical-gauss
  ([]
   (gen-edges-diag 0.05 0.1 0.9 0.1 0.9))
  ([step]
   (gen-edges-diag step 0.1 0.9))
  ([step s e]
   (gen-edges-diag step e e s s)) ;this is weird! something to do with directionality of intersections I am guessing.  best just specify xs xe ys ye until fully understood!
  ([step xs xe ys ye x-sd y-sd edge x-offset]
   (let [p1 (tv/vec3 [xs ys 0])
         p2 (tv/vec3 [xs ye 0])
         p3 (tv/vec3 [xe ys 0])
         p4 (tv/vec3 [xe ye 0])
         r (into [] (range 0 1.0 step))
         s-east (mapv #(gauss-3d (tu/point-at-index [p1 p2] %) x-sd y-sd 0 edge x-offset) r)
         e-east (mapv #(gauss-3d (tu/point-at-index [p3 p4] %) x-sd y-sd 0 edge x-offset) r)
         s-north (mapv #(gauss-3d (tu/point-at-index [p2 p4] %) x-sd y-sd 0 edge x-offset) r)
         e-north (mapv #(gauss-3d (tu/point-at-index [p1 p3] %) x-sd y-sd 0 edge x-offset) r)]
     {:s-east s-east :e-east e-east :s-north s-north :e-north e-north})))
