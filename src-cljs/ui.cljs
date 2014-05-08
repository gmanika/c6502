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

(defn tag-byte
  [cpu addr byte]
  (if (= (:pc cpu) addr)
    {:addr addr :byte byte :tag "pc"}
    {:addr addr :byte byte}))


(defn tag-bytes
  [cpu]
  (map-indexed #(tag-byte cpu %1 %2) (:memory cpu)))



(defn format-bytes
  [bytes]
  (apply str (map #(str "<span class='" (:tag %1) "'>" (hex (:byte %1)) "</span>") bytes)))

(defn format-memory-line
  [bytes]
  (str (zero-pad (:addr (first bytes))) ":  " (string/join " " (map format-bytes (partition 2 bytes)))))

(format-memory-line [{:byte 1 :tag :pc :addr 0} {:byte 2}  {:byte 3} {:byte 4}])

(defn format-dump
  [cpu]
  (apply str (map #(str "<pre>" %1 "</pre>") (map format-memory-line (partition 8 (tag-bytes cpu))))))


(defn render
  [cpu]
  (let [firstreg (.getElementById js/document "firstreg")
        secondreg (.getElementById js/document "secondreg")
        memoryarea (.getElementById js/document "memory")]
    (set! (. firstreg -innerHTML) (format-registers cpu :ac :xr :yr))
    (set! (. secondreg -innerHTML) (format-registers cpu :sp :sr :pc))
    (set! (. memoryarea -innerHTML) (format-dump cpu))))
