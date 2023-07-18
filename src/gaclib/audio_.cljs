(ns gaclib.audio
  (:require [goog.string :as gstring]
            [goog.string.format]
            [goog.dom :as dom]
            [goog.events :as events]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; audio stuff via web audio api
;;; following https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Visualizations_with_Web_Audio_API


;; defonce should mean multiple audio contexts aren't created - seems like a good iea, although no hard barrier hit so far asaik
(defonce audioCtx (new js/AudioContext))

;; using defonce here and on track means vol doesn't build up on multiple save swithin figwheel - super important!
(defonce analyser (.createAnalyser audioCtx))

(set! (.-fftSize analyser) 2048)
(def buffer-length (.-frequencyBinCount analyser))
(def data-array (js/Uint8Array. buffer-length))
(defn get-byte-ts []
  (.getByteTimeDomainData analyser data-array)
  data-array)


;; set up a second analyser for freq domain - maybe redundant!
(defonce analyser-freq (.createAnalyser audioCtx))

(set! (.-fftSize analyser-freq) 256)
(def buffer-length-freq (.-frequencyBinCount analyser-freq))
(def data-array-freq (js/Uint8Array. buffer-length-freq))
(defn get-byte-fd []
  (.getByteFrequencyData analyser-freq data-array-freq)
  data-array-freq)


;; get the audio element - defonce important not to build up volume on save and reloads in figwheel
(defonce audio-element (.querySelector js/document "audio"))
;; pass it to the audio context
(defonce track (.createMediaElementSource audioCtx audio-element))


;; select play button!
;; note this requires button element to be written in index.html from get go
;; cljs/img includes code snippet to do this all from cljs file, and also uses "this-as" 
(defonce play-button (.querySelector js/document "button"))

;; what to do when clicked!
(defn track-play-pause []
  ;; check if context is in suspended state (autoplay policy)
  (if (= (.-state audioCtx) "suspended")
    (.resume audioCtx))

  ;; play/pause track depending on state
  (if (= (.. play-button -dataset -playing) "false")
    (do 
      (.play audio-element)
      ;(set! (.. play-button -dataset -playing) "true"))
      (set! (.-playing (.-dataset play-button)) "true")) ;identical to line above, without .. macro
    (do
      (.pause audio-element)
      (set! (.. play-button -dataset -playing) "false"))))

(defn track-ended []
  (set! (.. play-button -dataset -playing) "false"))

;; idiomatic way to listen to a button click on an element
;; from : https://gist.github.com/djKianoosh/7443427
;;;;;;;

(defn log [msg]
  (js/console.log msg))

(defn listen [el type f]
  (let [e (dom/getElement el)]
    ;(events/listen e type (partial f))
    (events/listen e type f) ; above line with partial is original but doesn't seem necessay at least for track-play-paus and track-ended (perhaps needed for log)
    ))

(listen play-button "click" track-play-pause)
(listen audio-element "ended" track-ended)
;(listen play-button "click" log)
;;;;;;;


;;; AUDIO CHAIN

;; connect track to speaker (no analyser just yet)
;(.connect track (.-destination audioCtx))

;; and HERE'S THE MAGIC BIT WHERE ANALYSER IS CONNECTED!

;; just connect analyser hanging off track on its own (no need for a blackhole like in ChucK!)
;(.connect track analyser)

;; or connect link analyser onwards to speaker (only needed if track / audio chain not already connected to speakers)
;; mind that this doesn't double the amplitude to speakers, even though that isn't  - I think it does assuming that tracker -> destination ca line 28 above!
(.connect (.connect track analyser) (.-destination audioCtx))

;; connect analyser-freq but don't bother linking to destination, as that would double the track and volume...
(.connect track analyser-freq)
