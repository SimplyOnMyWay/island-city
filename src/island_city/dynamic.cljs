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
  (q/no-stroke)
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


  (def amp 0.75)
  (def omega 0.02)  
  (def alpha1 (* amp (q/sin (+  0 (* omega (:cntr state))))))
  (def alpha2 (* amp (q/sin (+ 10 (* omega (:cntr state))))))
  (def alpha3 (* amp (q/sin (+ 20 (* omega (:cntr state))))))
  (def alpha4 (* amp (q/cos (+ 30 (* omega (:cntr state))))))
  (def alpha5 (* amp (q/sin (+ 40 (* omega (:cntr state))))))
  (def alpha6 (* amp (q/sin (+ 50 (* omega (:cntr state))))))
  (def alpha7 (* amp (q/sin (+ 60 (* omega (:cntr state))))))
  (def alpha8 (* amp (q/sin (+ 70 (* omega (:cntr state))))))      

  (def alpha 1.0)

  (def pal1 [[355, 78, 85, alpha1],
             [169, 75, 65, alpha2],
             [39, 59, 95, alpha3],
             [22, 44, 35, alpha4],
             [227, 65, 20, alpha5],
             [10, 35, 97, alpha6],
             [25, 77, 90, alpha7],
             [40, 12, 88, alpha8]])
  
  (def pscott-rosc-extend [[208, 65, 74, alpha],
                           [107, 60, 45, alpha],
                           [79, 83, 74, alpha],
                           [0, 76, 94, alpha],
                           [208, 65, 74, alpha],
                           [107, 60, 45, alpha],
                           [79, 83, 74, alpha],
                           [0, 76, 94, alpha]]),


  
  
  ;; adjust for tiny offset 
  (q/translate (w -0.0032) 0)

;  (q/stroke [0 100 30 1])
;  (q/line (w 0.5) (h 0) (w 0.5) (h 1.0))

  ;; top trianglar yoke
;  (q/fill [0 100 30 alpha1])
  (q/fill (pal1 0))
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
  ;; (q/fill [50 100 50 alpha2])
  (q/fill (pal1 1))
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
<
  ;; inner face of top triangular yoke
;;  (q/fill [80 70 90 alpha3])
  (q/fill (pal1 2))  
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
;;  (q/fill [100 70 90 alpha4])
  (q/fill (pal1 3))
  (q/begin-shape)
  (doseq [theta (map #(* pi %) (range 1 2.01 0.01))]
    (q/vertex (w (+ 0.5 (* 0.069 (q/cos theta)))) (h (+ 0.32 (* 0.123 (q/sin theta))))))
  (q/begin-contour)
  (doseq [theta (map #(* pi %) (reverse (range 1 2.01 0.01)))]
    (q/vertex (w (+ 0.5 (* 0.06 (q/cos theta)))) (h (+ 0.32 (* 0.11 (q/sin theta))))))
  (q/end-contour)  
  (q/end-shape)

  ;; filled semi circular
;;  (q/fill [180 70 90 alpha5])
  (q/fill (pal1 4))
  (q/begin-shape)
  (doseq [theta (map #(* pi %) (range 1 2.01 0.01))]
    (q/vertex (w (+ 0.5 (* 0.06 (q/cos theta)))) (h (+ 0.32 (* 0.11 (q/sin theta))))))
  (q/end-shape)

  ;; ;;;;;;;;;;;
  ;; first floor
  ;; ;;;;;;;;;;;  
  ;;(q/fill [100 100 30 0.5])
  (q/fill (pal1 4)) 
  ;; center
  (q/begin-shape)
  (q/vertex (w 0.441) (h 0.376))
  (q/vertex (w 0.56) (h 0.376))
  (q/vertex (w 0.56) (h 0.576))
  (q/vertex (w 0.538) (h 0.576))  
  (q/vertex (w 0.538) (h 0.568))
  (q/vertex (w 0.465) (h 0.568))
  (q/vertex (w 0.465) (h 0.576))      
  (q/vertex (w 0.441) (h 0.576))
  (q/begin-contour)
  (q/vertex (w 0.483) (h 0.568))
  (q/vertex (w 0.517) (h 0.568))
  (q/vertex (w 0.517) (h 0.445))
  (doseq [theta (map #(* pi %) (reverse (range 1 2.01 0.01)))]
    (q/vertex (w (+ 0.5 (* 0.016 (q/cos theta)))) (h (+ 0.445 (* 0.027 (q/sin theta))))))    
  (q/vertex (w 0.483) (h 0.445))
  (q/end-contour)  
  (q/end-shape)  

;;  (q/fill [30 100 80 0.5])
  (q/fill (pal1 6))  

  ;; left
  (q/push-matrix)
  (q/translate (w -0.163) (h 0))
  (q/begin-shape)
  (q/vertex (w 0.413) (h 0.375))
  (q/vertex (w 0.603) (h 0.375))
  (q/vertex (w 0.603) (h 0.57))
  (q/vertex (w 0.413) (h 0.57))  
  (q/begin-contour)
  (q/vertex (w 0.483) (h 0.568))
  (q/vertex (w 0.517) (h 0.568))
  (q/vertex (w 0.517) (h 0.445))
  (doseq [theta (map #(* pi %) (reverse (range 1 2.01 0.01)))]
    (q/vertex (w (+ 0.5 (* 0.016 (q/cos theta)))) (h (+ 0.445 (* 0.027 (q/sin theta))))))    
  (q/vertex (w 0.483) (h 0.445))
  (q/end-contour)
  (q/end-shape)
  (q/pop-matrix)


  ;; right
  (q/push-matrix)
  (q/translate (w 0.163) (h 0))
  (q/begin-shape)
  (q/vertex (w 0.398) (h 0.375))
  (q/vertex (w 0.586) (h 0.375))
  (q/vertex (w 0.586) (h 0.57))
  (q/vertex (w 0.398) (h 0.57))  
  (q/begin-contour)
  (q/vertex (w 0.483) (h 0.568))
  (q/vertex (w 0.517) (h 0.568))
  (q/vertex (w 0.517) (h 0.445))
  (doseq [theta (map #(* pi %) (reverse (range 1 2.01 0.01)))]
    (q/vertex (w (+ 0.5 (* 0.016 (q/cos theta)))) (h (+ 0.445 (* 0.027 (q/sin theta))))))    
  (q/vertex (w 0.483) (h 0.445))
  (q/end-contour)
  (q/end-shape)
  (q/pop-matrix)


  ;; ground level
 ;; (q/fill [200 100 30 0.3])
  (q/fill (pal1 7))
  (q/push-matrix)
  (q/translate (w -0.145) (h 0.195))
  (q/begin-shape)
  (q/vertex (w 0.398) (h 0.375))
  (q/vertex (w 0.54) (h 0.375))
  (q/vertex (w 0.54) (h 0.655))
  (q/vertex (w 0.398) (h 0.655))  
  (q/begin-contour)
  (q/vertex (w 0.465) (h 0.595))
  (q/vertex (w 0.5) (h 0.595))
  (q/vertex (w 0.5) (h 0.45))
  (q/vertex (w 0.465) (h 0.45))
  (q/end-contour)
  (q/end-shape)
  (q/pop-matrix)

  (q/push-matrix)
  (q/translate (w 0.208) (h 0.195))
  (q/begin-shape)
  (q/vertex (w 0.398) (h 0.375))
  (q/vertex (w 0.54) (h 0.375))
  (q/vertex (w 0.54) (h 0.655))
  (q/vertex (w 0.398) (h 0.655))  
  (q/begin-contour)
  (q/vertex (w 0.438) (h 0.595))
  (q/vertex (w 0.472) (h 0.595))
  (q/vertex (w 0.472) (h 0.45))
  (q/vertex (w 0.438) (h 0.45))
  (q/end-contour)
  (q/end-shape)
  (q/pop-matrix)
  

  ;; columns front ground level
  ;;  (q/fill [60 100 90 0.1])
  (q/fill (pal1 0)) 
  (q/rect (w 0.395) (h 0.61) (w 0.21) (h 0.015))
  (q/rect (w 0.395) (h 0.625) (w 0.21) (h 0.005))
  (q/rect (w 0.395) (h 0.63) (w 0.21) (h 0.005))
  (q/rect (w 0.395) (h 0.635) (w 0.21) (h 0.005))      
  (q/rect (w 0.395) (h 0.64) (w 0.013) (h 0.2))
  (q/rect (w 0.46) (h 0.64) (w 0.013) (h 0.2))
  (q/rect (w 0.527) (h 0.64) (w 0.013) (h 0.2))
  (q/rect (w 0.592) (h 0.64) (w 0.013) (h 0.2)))  
  
(comment
  (q/with-stroke [0 100 100 1.0]
    (q/stroke-weight (w 0.01))
    (q/rect 0 0 (w 1.0) (h 1.0)))
  (q/with-fill [0 100 100 1.0]
    (q/rect (w 0.1) (h 0.2) (w 0.8) (h 0.6) )))

