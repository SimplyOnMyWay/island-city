(ns gaclib.time
  (:require [goog.date :as gd]))

(defn curr-timestamp
  []
  (let [ ;; google closure reformats nicely and removes a load of confusion - should be robust across browsers too!
        date (gd/DateTime. js/Date.)
        ;;all not used but ref for future use...
        cmill (.getMilliseconds date)
        csec (.getSeconds date)
        cmin (.getMinutes date)
        chour (.getHours date) 
        cday (.getDate date)
        cmon (inc (.getMonth date))
        cyear (.getFullYear date)
        curr-date (str cyear "-" cmon "-" cday " "chour ":" cmin ":"csec ":" cmill)
        time-stamp (str date cmill)] 
    time-stamp))

(defn milli-time
  "Return milliseconds since 'start of epoch', 01.01.1970
   Ref: https://google.github.io/closure-library/api/goog.date.DateTime.html

   NOTE: could use (.now (.-performance js/window)) in future to strip off a sub millisecond time, as it seems to allow decimals of ms, even though documentation doesn't seem to ref this"
  []
  (let [now (.getTime (gd/DateTime. gd/Date))]
    now))

(defn milli-time-old
  "not suitable as gets ms since start of session, not epoch"
  []
  (let [now (.now (.-performance js/window))]
    now))


