(ns gaclib.junction-art
  (:require [quil.core :as q]
            [gaclib.vector-adventures :as v]
            [gaclib.gui :refer [w h hw]]
            [gaclib.line-ops :refer [gappify-all-roads gappify-all-roads-reverse]]
            [thi.ng.geom.core :as tg]
            [thi.ng.geom.vector :as tv]
            [thi.ng.math.core :as tm]))

(defn nudge-e-
  [p east-road north-road]
  (let [s (:s east-road)
        e (:e east-road)
        dr (tm/- e s)
        dr-norm (tm/normalize dr)
        p-nudged (tm/- p (tg/scale dr-norm (/ (:rs north-road) 2)))
        ]
    p-nudged))

(defn nudge-e+
  [p east-road north-road]
  (let [s (:s east-road)
        e (:e east-road)
        dr (tm/- e s)
        dr-norm (tm/normalize dr)
        p-nudged (tm/+ p (tg/scale dr-norm (/ (:rs north-road) 2)))
        ]
    p-nudged))

(defn nudge-n-
  "this is a bit funky - I am clear how east-road and north-road are treated but the tm/+ is peculiar since opposite to the sign of the function name ie 'nudge-n-.  May need anther look at how used in high-low road below.  For now it is working!"
  [p east-road north-road]
  (let [s (:s north-road)
        e (:e north-road)
        dr (tm/- e s)
        dr-norm (tm/normalize dr)
        p-nudged (tm/+ p (tg/scale dr-norm (/ (:rs east-road) 2)))
        ]
    p-nudged))

(defn nudge-n+
[p east-road north-road]
  (let [s (:s north-road)
        e (:e north-road)
        dr (tm/- e s)
        dr-norm (tm/normalize dr)
        p-nudged (tm/- p (tg/scale dr-norm (/ (:rs east-road) 2)))
        ]
    p-nudged))

(defn high-low
  "this is my first effort at junction art, ie how to make beautiful things happen at the junctions between the east and north roads.  this one is a simple alternating high low road pattern, inspired by early Irish knot-work"
  ([east-roads north-roads]
   (high-low east-roads north-roads nil))
  ([east-roads north-roads verbose?]
   "set verbose? to :v to print draw status at each junction"
   (let [i-e-n (gappify-all-roads east-roads north-roads)
         i-e-n-part (v/vec-partition (count north-roads) i-e-n)
         i-n-e (gappify-all-roads-reverse east-roads north-roads)
         i-n-e-part (v/vec-partition (count east-roads) i-n-e)]
     
     ;; east road juntion art!
     (doseq [road-no (range (count i-e-n-part))]
       (cond (even? road-no)
             (doseq [jn-no (range 0 (count (i-e-n-part road-no)) 2)]
               (if (= (((i-e-n-part road-no) jn-no) 0) [])
                 (if (= verbose? :v)
                   (println (str "\nEAST EVEN junction skipped!: :intersection-outside at ... "
                                 "\nroad-no = " road-no
                                 " jn-no = " jn-no
                                 "\n")))
                 (do
                   (if (= verbose? :v)
                     (println (str "\nEAST EVEN junction drawn!!!: :intersection at ... "
                                   "\nroad-no = " road-no
                                   " jn-no = " jn-no
                                   "\n")))
                   (v/vline (nudge-e- (((i-e-n-part road-no) jn-no) 0) (east-roads road-no) (north-roads jn-no)) (nudge-e+ (((i-e-n-part road-no) jn-no) 2) (east-roads road-no) (north-roads jn-no)) (:rc (east-roads road-no)) (:rs (east-roads road-no)))
                   (v/vline (((i-e-n-part road-no) jn-no) 4)  (((i-e-n-part road-no) jn-no) 6) (:pc (east-roads road-no)) (:ps (east-roads road-no)))
                   (v/vline (((i-e-n-part road-no) jn-no) 5)  (((i-e-n-part road-no) jn-no) 7) (:pc (east-roads road-no)) (:ps (east-roads road-no)))
                   (v/vline (nudge-e- (((i-e-n-part road-no) jn-no) 1) (east-roads road-no) (north-roads jn-no))  (nudge-e+ (((i-e-n-part road-no) jn-no) 3) (east-roads road-no) (north-roads jn-no)) (:rc (east-roads road-no)) (:rs (east-roads road-no))))))
             :else
             (doseq [jn-no (range 1 (count (i-e-n-part road-no)) 2)]
               (if (= (((i-e-n-part road-no) jn-no) 0) [])
                 (if (= verbose? :v)
                   (println (str "\nEAST ODD junction skipped!: :intersection-outside at ... "
                                 "\nroad-no = " road-no
                                 " jn-no = " jn-no
                                 "\n")))
                 (do
                   (if (= verbose? :v)
                     (println (str "\nEAST ODD junction drawn!!!: :intersection at ... "
                                   "\nroad-no = " road-no
                                   " jn-no = " jn-no
                                   "\n")))
                   (v/vline (nudge-e- (((i-e-n-part road-no) jn-no) 0) (east-roads road-no) (north-roads jn-no)) (nudge-e+ (((i-e-n-part road-no) jn-no) 2) (east-roads road-no) (north-roads jn-no)) (:rc (east-roads road-no)) (:rs (east-roads road-no)))
                   (v/vline (((i-e-n-part road-no) jn-no) 4)  (((i-e-n-part road-no) jn-no) 6) (:pc (east-roads road-no)) (:ps (east-roads road-no)))
                   (v/vline (((i-e-n-part road-no) jn-no) 5)  (((i-e-n-part road-no) jn-no) 7) (:pc (east-roads road-no)) (:ps (east-roads road-no)))
                   (v/vline (nudge-e- (((i-e-n-part road-no) jn-no) 1) (east-roads road-no) (north-roads jn-no)) (nudge-e+ (((i-e-n-part road-no) jn-no) 3) (east-roads road-no) (north-roads jn-no)) (:rc (east-roads road-no)) (:rs (east-roads road-no))))))))

     ;; north road junction art!
     (doseq [road-no (range (count i-n-e-part))]
       (cond (even? road-no)
             (doseq [jn-no (range 1 (count (i-n-e-part road-no)) 2)]
               (if (= (((i-n-e-part road-no) jn-no) 0) [])
                 (if (= verbose? :v)
                   (println (str "\nNORTH EVEN junction skipped!: :intersection-outside at ... "
                                 "\nroad-no = " road-no
                                 " jn-no = " jn-no
                                 "\n")))
                 (do
                   (if (= verbose? :v)
                     (println (str "\nNORTH EVEN junction drawn!!!: :intersection at ... "
                                   "\nroad-no = " road-no
                                   " jn-no = " jn-no
                                   "\n")))
                   (v/vline (nudge-n- (((i-n-e-part road-no) jn-no) 0) (east-roads jn-no) (north-roads road-no)) (nudge-n+ (((i-n-e-part road-no) jn-no) 2) (east-roads jn-no) (north-roads road-no)) (:rc (north-roads road-no)) (:rs (north-roads road-no)))
                   (v/vline (((i-n-e-part road-no) jn-no) 4)  (((i-n-e-part road-no) jn-no) 6) (:pc (north-roads road-no)) (:ps (north-roads road-no)))
                   (v/vline (((i-n-e-part road-no) jn-no) 5)  (((i-n-e-part road-no) jn-no) 7) (:pc (north-roads road-no)) (:ps (north-roads road-no)))
                   (v/vline (nudge-n- (((i-n-e-part road-no) jn-no) 1) (east-roads jn-no) (north-roads road-no)) (nudge-n+ (((i-n-e-part road-no) jn-no) 3) (east-roads jn-no) (north-roads road-no)) (:rc (north-roads road-no)) (:rs (north-roads road-no))))))
             :else
             (doseq [jn-no (range 0 (count (i-n-e-part road-no)) 2)]
               (if (= (((i-n-e-part road-no) jn-no) 0) [])
                 (if (= verbose? :v)
                   (println (str "\nNORTH ODD junction skipped!: :intersection-outside at ... "
                                 "\nroad-no = " road-no
                                 " jn-no = " jn-no
                                 "\n")))
                 (do
                   (if (= verbose? :v)
                     (println (str "\nNORTH ODD junction drawn!: :intersection at ... "
                                   "\nroad-no = " road-no
                                   " jn-no = " jn-no
                                   "\n")))
                   (v/vline (nudge-n- (((i-n-e-part road-no) jn-no) 0) (east-roads jn-no) (north-roads road-no)) (nudge-n+ (((i-n-e-part road-no) jn-no) 2) (east-roads jn-no) (north-roads road-no)) (:rc (north-roads road-no)) (:rs (north-roads road-no)))
                   (v/vline (((i-n-e-part road-no) jn-no) 4)  (((i-n-e-part road-no) jn-no) 6) (:pc (north-roads road-no)) (:ps (north-roads road-no)))
                   (v/vline (((i-n-e-part road-no) jn-no) 5)  (((i-n-e-part road-no) jn-no) 7) (:pc (north-roads road-no)) (:ps (north-roads road-no)))
                   (v/vline (nudge-n- (((i-n-e-part road-no) jn-no) 1) (east-roads jn-no) (north-roads road-no)) (nudge-n+ (((i-n-e-part road-no) jn-no) 3) (east-roads jn-no) (north-roads road-no)) (:rc (north-roads road-no)) (:rs (north-roads road-no)))))))))))



    

