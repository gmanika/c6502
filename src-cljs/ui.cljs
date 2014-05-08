(ns c6502.ui
  (:require [clojure.string :as string]))

(def hexchars ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"])

(defn zero-pad
  [n]
  (if (< (.-length (str n)) 4)
    (zero-pad (str "0" n))
     n))

(defn format-registers
  [cpu & regs]
  (string/join " " (map #(str (name %1) " " (zero-pad (%1 cpu))) regs)))

(defn hex
  [byte]
  (let [q (quot byte 16)
        r (rem byte 16)]
    (str (nth hexchars (rem q 16)) (nth hexchars r))))

(defn format-bytes
  [bytes]
  (str (string/join " " (map hex bytes))))

(defn format-dump
  [cpu]
  (let [memory (:memory cpu)]
    (apply str (map-indexed #(str "<pre>" (zero-pad %1) ":  " (format-bytes %2) "</pre>") (partition 8 memory)))))

(defn render
  [cpu]
  (let [firstreg (.getElementById js/document "firstreg")
        secondreg (.getElementById js/document "secondreg")
        memoryarea (.getElementById js/document "memory")]
    (set! (. firstreg -innerHTML) (format-registers cpu :ac :xr :yr))
    (set! (. secondreg -innerHTML) (format-registers cpu :sp :sr :pc))
    (set! (. memoryarea -innerHTML) (format-dump cpu))))
