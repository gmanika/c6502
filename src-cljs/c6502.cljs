(ns c6502
  (:require [c6502.ui :as ui]))

(defn reset-memory
  []
  (vec (repeat 65536 0)))

(defrecord CPU
  [ac xr yr sp sr pc memory])

(def running-cpu
  (atom (CPU. 0 1 2 3 4 5 (reset-memory))))

(defn step
  [cpu])

(defn main
  []
  (c6502.ui/render @running-cpu))

