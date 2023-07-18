(ns island-city.dynamic
  (:require [quil.core :as q :include-macros true]))

(defn h
  ([] (h 1.0))
  ([value] (* (q/height) value)))

(comment
  (defn w
    ([] (w 1.0))
    ([value] (* (q/width) (* (/ 1080 1920) value)))))

(defn w
  ([] (w 1.0))
  ([value] (* (q/width) value)))

(def pi Math/PI)

;; main quil functions
(defn setup
  []
                                        ;  (q/no-loop)
  ;;  (q/no-stroke)
  (q/stroke-weight (w 0.0005))
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100 1.0)
                                        ;  (q/rect-mode :center)
  (q/ellipse-mode :center)
  {:image-name (q/load-image "/images/triskel-front-5.png")
   :cntr 0})

(defn update-state
  [state]
  {:image-name (:image-name state)
   :cntr  (inc (:cntr state))})

(defn draw
  [state]
  (let [im (:image-name state)]
    (when (q/loaded? im)
      (q/image im 0 0 (w 1.0) (h 1.0))))

  (def alpha1 (/ (rem (* 0.4 (:cntr state)) 10) 10))
  (def alpha2 (/ (rem (+ (* 0.3 (:cntr state)) 20) 10) 10))
  (def alpha3 (/ (rem (+ (* 0.1 (:cntr state)) 40) 10) 10))  
  
  ;; adjust for tiny offset 
  (q/translate (w -0.0032) 0)

;  (q/stroke [0 100 30 1])
;  (q/line (w 0.5) (h 0) (w 0.5) (h 1.0))

  ;; top trianglar yoke
  (q/fill [0 100 30 alpha1])
  (q/begin-shape)
  (q/vertex (w 0.5) (h 0.04))
  (q/vertex (w 0.225) (h 0.315))
  (q/vertex (w 0.225) (h 0.33))  
  (q/vertex (w 0.775) (h 0.33))
  (q/vertex (w 0.775) (h 0.315))
  (q/begin-contour)
  (q/vertex (w 0.5) (h 0.06))
  (q/vertex (w 0.76) (h 0.32))
  (q/vertex (w 0.237) (h 0.32))
  (q/end-contour)  
  (q/end-shape)
  ;; inner top trianglar yoke!
  (q/fill [50 100 50 alpha2])
  (q/begin-shape)
  (q/vertex (w 0.5) (h 0.06))
  (q/vertex (w 0.237) (h 0.32))
  (q/vertex (w 0.76) (h 0.32))
  (q/begin-contour)
  (q/vertex (w 0.5) (h 0.087))
  (q/vertex (w 0.737) (h 0.32))
  (q/vertex (w 0.263) (h 0.32))
  (q/end-contour)  
  (q/end-shape)

  ;; inner face of top triangular yoke
  (q/fill [80 70 90 alpha3])
  (q/begin-shape)
  (q/vertex (w 0.5) (h 0.087))
  (q/vertex (w 0.263) (h 0.32))
  (q/vertex (w 0.737) (h 0.32))
  (q/begin-contour)
  (doseq [theta (map #(* pi %) (range 1 2.01 0.01))]
    (q/vertex (w (+ 0.5 (* 0.069 (q/cos theta)))) (h (+ 0.32 (* 0.123 (q/sin theta))))))
  (q/end-contour)  
  (q/end-shape)

  ;; semi circular arc
  (q/fill [100 70 90 0.5])
  (q/begin-shape)
  (doseq [theta (map #(* pi %) (range 1 2.01 0.01))]
    (q/vertex (w (+ 0.5 (* 0.069 (q/cos theta)))) (h (+ 0.32 (* 0.123 (q/sin theta))))))
  (q/begin-contour)
  (doseq [theta (map #(* pi %) (reverse (range 1 2.01 0.01)))]
    (q/vertex (w (+ 0.5 (* 0.06 (q/cos theta)))) (h (+ 0.32 (* 0.11 (q/sin theta))))))
  (q/end-contour)  
  (q/end-shape)

  ;; filled semi circular
  (q/fill [180 70 90 0.5])
  (q/begin-shape)
  (doseq [theta (map #(* pi %) (range 1 2.01 0.01))]
    (q/vertex (w (+ 0.5 (* 0.06 (q/cos theta)))) (h (+ 0.32 (* 0.11 (q/sin theta))))))
  (q/end-shape)

  ;; first floor
  (q/fill [100 100 30 0.3])
  ;; center
  (q/rect (w 0.4) (h 0.36) (w 0.205) (h 0.21))
  (q/fill [30 100 80 0.3])
  ;; left
  (q/rect (w 0.25) (h 0.36) (w 0.150) (h 0.21))
  ;; right
  (q/rect (w 0.605) (h 0.36) (w 0.150) (h 0.21))

  ;; ground level
  (q/fill [200 100 30 0.3])
  (q/rect (w 0.25) (h 0.57) (w 0.07) (h 0.28))  
  (q/rect (w 0.685) (h 0.57) (w 0.07) (h 0.28))  


  ;; columns front ground level
  (q/fill [60 100 90 0.1])
  (q/rect (w 0.395) (h 0.61) (w 0.21) (h 0.015))
  (q/rect (w 0.395) (h 0.625) (w 0.21) (h 0.005))
  (q/rect (w 0.395) (h 0.63) (w 0.21) (h 0.005))
  (q/rect (w 0.395) (h 0.635) (w 0.21) (h 0.005))      
  (q/rect (w 0.395) (h 0.64) (w 0.013) (h 0.2))
  (q/rect (w 0.46) (h 0.64) (w 0.013) (h 0.2))
  (q/rect (w 0.527) (h 0.64) (w 0.013) (h 0.2))
  (q/rect (w 0.592) (h 0.64) (w 0.013) (h 0.2))  
  
  (comment
    (q/with-stroke [0 100 100 1.0]
      (q/stroke-weight (w 0.01))
      (q/rect 0 0 (w 1.0) (h 1.0)))
    (q/with-fill [0 100 100 1.0]
      (q/rect (w 0.1) (h 0.2) (w 0.8) (h 0.6) )))
)
