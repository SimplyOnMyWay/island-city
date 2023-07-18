(ns gaclib.gui
  (:require [quil.core :as q :include-macros true]))

(defn h
  ([] (h 1.0))
  ([value] (* (q/height) value)))

(comment
  (defn w
    ([] (w 1.0))
    ([value] (* (q/width) (* (/ 1080 1920) value)))))

(comment)
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

(def grid-colour [240 0 100 0.5])
(def grid-spacing 10)
(def grid-stroke-weight 1)
(def body-step 20)


(defn eye-pos
  "return eye position in cartesian coordinate component given the x- y- or z- component value 
NOTE: (h 0.5) may need a rethink"
  [comp]
  (/ (h 0.5) (q/tan (/ (* q/PI comp) 360.0))))

(defn cam-xyz
  "translate camera with respect to screen width and heigth, remaining perpindicular to x-z plane"
([x y z]
 (cam-xyz x y z :y))
  ([x y z ax]
   (cond
     (= ax :x)
     (let [eyeX x
           eyeY y 
           eyeZ (eye-pos (* -1 z))   
           centerX x
           centerY y
           centerZ 0
           upX 1
           upY 0
           upZ 0]
       (q/camera eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ))
     (= ax :x-school)
     (let [eyeX x
           eyeY y 
           eyeZ (eye-pos (* 1 z))   
           centerX x
           centerY y
           centerZ 0
           upX -1
           upY 0
           upZ 0]
       (q/camera eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ))
     (= ax :y)
     (let [eyeX x
           eyeY y 
           eyeZ (eye-pos z)   
           centerX x
           centerY y
           centerZ 0
           upX 0
           upY 1
           upZ 0]
       (q/camera eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ))
     (= ax :y-school)
     (let [eyeX x
           eyeY y 
           eyeZ (eye-pos (* -1 z))   
           centerX x
           centerY y
           centerZ 0
           upX 0
           upY -1
           upZ 0]
       (q/camera eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ))
     (= ax :z)
     (let [eyeX x
           eyeY (eye-pos (* -1 y))   
           eyeZ z
           centerX x
           centerY 0
           centerZ z
           upX 0
           upY 0
           upZ 1]
       (q/camera eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ))
     (= ax :z-school)
     (let [eyeX x
           eyeY (eye-pos (* 1 y))   
           eyeZ z
           centerX x
           centerY 0
           centerZ z
           upX 0
           upY 0
           upZ -1]
       (q/camera eyeX eyeY eyeZ centerX centerY centerZ upX upY upZ)))))

(defn draw-coord-vectors
  "visual aid to show coord unit vectors XYZ"
  [ox oy oz unit-length]
  (let [po [ox oy oz]
        px [(+ ox unit-length) oy oz]
        py [ox (+ oy unit-length) oz]
        pz [ox oy (+ oz unit-length)]
        stroke-weight 0.005
        sphere-radius 0.01]
    (q/stroke-weight (w stroke-weight))
    (q/with-stroke [0 100 100 1.0]
      (q/line po px)
      (q/with-translation (into [] (map + po [unit-length 0 0]))
        (q/sphere (w sphere-radius))))
    (q/with-stroke [120 100 100 1.0]
      (q/line po py)
      (q/with-translation (into [] (map + po [0 unit-length 0 ]))
        (q/sphere (w sphere-radius))))
    (q/with-stroke [240 100 100 1.0]
      (q/line po pz)
      (q/with-translation (into [] (map + po [0 0 unit-length]))
        (q/sphere (w sphere-radius))))))

(defn initialise-gui-state
  [ax]
  (into {:key-down false :key-mem ""}
        (cond
          (re-find #"x" (str ax))
          {:ax ax
           :cam-x (w 0.5)
           :cam-y (h 0.5)
           :cam-z 60}
          (re-find #"y" (str ax))
          {:ax ax
           :cam-x (w 0.5)
           :cam-y (h 0.5)
           :cam-z 60}
          (re-find #"z" (str ax))
          {:ax ax
           :cam-x (w 0.5)
           :cam-y 60
           :cam-z (h 0.5)})))


(defn update-gui-state
  "update gui state and return"
  ([state]
   (update-gui-state state 0.01))
  ([state cam-step]
   (update-gui-state state cam-step cam-step cam-step))
  ([state cam-xy-step cam-z-step]
   (update-gui-state state cam-xy-step cam-xy-step cam-z-step))
  ([state cam-x-step cam-y-step cam-z-step]
   (cond
     (and (:key-down state) (= (str (q/raw-key)) "l") true) (update-in state [:cam-x] + (w cam-x-step))
     (and (:key-down state) (= (str (q/raw-key)) "h") true) (update-in state [:cam-x] - (w cam-x-step))
     (and (:key-down state) (= (str (q/raw-key)) "k") true) (update-in state [:cam-y] + (h cam-y-step))
     (and (:key-down state) (= (str (q/raw-key)) "j") true) (update-in state [:cam-y] - (h cam-y-step))
     (and (:key-down state) (= (str (q/raw-key)) "a") true) (update-in state [:cam-z] + (w cam-z-step))
     (and (:key-down state) (= (str (q/raw-key)) "z") true) (update-in state [:cam-z] - (w cam-z-step))
     (and (:key-down state)) (assoc-in state [:key-mem] (str (q/raw-key)))
     :else state)))

(comment
  ;;text can only take xy in clojurescript
  (defn text-selfie-stick
    "text behaves like camera is holding it on a selfie stick. BETA!!"
    [text-str cam-x cam-y cam-z]
    (q/with-rotation [(q/radians 90) 1 0 0] (q/text text-str (- cam-x 250) (- (eye-Y cam-y) 0) (- 0 cam-z)))))

(defn body-x-conds
  "change :body-x values for various bodys using key combos"
  [state]
  (cond
    (and (:key-down state) (= (:key-mem state) "1") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v1] + body-step)
    (and (:key-down state) (= (:key-mem state) "1") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v1] - body-step)
    (and (:key-down state) (= (:key-mem state) "1") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v1] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "1") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v1] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "2") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v2] + body-step)
    (and (:key-down state) (= (:key-mem state) "2") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v2] - body-step)
    (and (:key-down state) (= (:key-mem state) "2") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v2] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "2") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v2] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "3") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v3] + body-step)
    (and (:key-down state) (= (:key-mem state) "3") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v3] - body-step)
    (and (:key-down state) (= (:key-mem state) "3") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v3] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "3") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v3] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "4") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v4] + body-step)
    (and (:key-down state) (= (:key-mem state) "4") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v4] - body-step)
    (and (:key-down state) (= (:key-mem state) "4") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v4] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "4") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v4] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "5") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v5] + body-step)
    (and (:key-down state) (= (:key-mem state) "5") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v5] - body-step)
    (and (:key-down state) (= (:key-mem state) "5") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v5] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "5") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v5] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "6") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v6] + body-step)
    (and (:key-down state) (= (:key-mem state) "6") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v6] - body-step)
    (and (:key-down state) (= (:key-mem state) "6") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v6] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "6") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v6] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "7") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v7] + body-step)
    (and (:key-down state) (= (:key-mem state) "7") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v7] - body-step)
    (and (:key-down state) (= (:key-mem state) "7") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v7] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "7") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v7] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "8") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v8] + body-step)
    (and (:key-down state) (= (:key-mem state) "8") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v8] - body-step)
    (and (:key-down state) (= (:key-mem state) "8") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v8] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "8") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v8] - (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "9") (= (str (q/raw-key)) "=") true) (update-in (:body-x state) [:v9] + body-step)
    (and (:key-down state) (= (:key-mem state) "9") (= (str (q/raw-key)) "-") true) (update-in (:body-x state) [:v9] - body-step)
    (and (:key-down state) (= (:key-mem state) "9") (= (str (q/raw-key)) "]") true) (update-in (:body-x state) [:v9] + (/ body-step 20))
    (and (:key-down state) (= (:key-mem state) "9") (= (str (q/raw-key)) "[") true) (update-in (:body-x state) [:v9] - (/ body-step 20))
    :else (:body-x state)))

(defn draw-xz-grid
  "draw grid at 20 x 20m spacing"
  [x-spacing z-spacing]
  ;; graphic attributes
  (q/stroke-weight grid-stroke-weight)
  ;; draw xy grid
  (q/with-stroke grid-colour
    (doseq [x (range 0 (q/width) x-spacing)]
      (q/line x 0 x (q/height)))
    (doseq [z (reverse (range 0 (q/height) z-spacing))]
      (q/line 0 z (q/width) z))))
