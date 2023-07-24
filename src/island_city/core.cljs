(ns ^:figwheel-hooks island-city.core
  (:require
   [goog.dom :as gdom]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [island-city.dynamic :as dynamic]))

(defn ^:export run-sketch []
  (q/defsketch island-city
    :host "island-city"
    :size [(/ 1920 1) (/ 1080 1)]
    :renderer :p3d
    :setup dynamic/setup
    :update dynamic/update-state
    :draw dynamic/draw
    :key-pressed
    (defn k-p [state {}]
      (assoc state :key-down true))
    :key-released
    (defn k-r [state {}]
      (assoc state :key-down false))
    :middleware [m/fun-mode]
    :settings #(q/smooth 4) ;don't think the 4 does anything in p5.js, perhaps only applies in processing
    ))

(run-sketch)


;;; FIGWHEEL-MAIN BOILERPLATE ...
;(println "This text is printed from src/island-city/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(defn get-app-element []
  (gdom/getElement "app"))



;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
