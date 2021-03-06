(ns ui
  (:require [clojure.string :as string]))

(def hexchars ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"])

(defn zero-pad
  ([n length]
   (if (< (.-length (str n)) length)
     (zero-pad (str "0" n) length)
      n))
   ([n]
    (zero-pad n 4)))


(defn format-registers
  [cpu & regs]
  (string/join " " (map #(str (name %1) " " (zero-pad (%1 cpu))) regs)))


(defn hex
  [byte]
  (let [q (quot byte 16)
        r (rem byte 16)]
    (str (if (zero? q) "" (hex q))(nth hexchars r))))


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
  (if (empty? (filter #(not (zero? %)) (map :byte bytes)))
    "..."
    (str (zero-pad (:addr (first bytes))) ":  " (string/join " " (map format-bytes (partition 2 bytes))))))

(format-memory-line [{:byte 1 :tag :pc :addr 0} {:byte 2}  {:byte 3} {:byte 4}])



(filter zero? (map :byte [{:byte 0} {byte 1}]))

(format-memory-line [{:byte 0} {:byte 1}])

(defn format-dump
  [cpu]
  (apply str (map first (partition-by identity (map #(str "<pre>" (format-memory-line %1) "</pre>")  (partition 16 (tag-bytes cpu)))))))


(defn render
  [cpu]
  (let [firstreg (.getElementById js/document "firstreg")
        secondreg (.getElementById js/document "secondreg")
        memoryarea (.getElementById js/document "memory")]
    (set! (. firstreg -innerHTML) (format-registers cpu :ac :xr :yr))
    (set! (. secondreg -innerHTML) (format-registers cpu :sp :sr :pc))
    (set! (. memoryarea -innerHTML) (format-dump cpu))))
