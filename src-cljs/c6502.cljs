(ns c6502
  (:require [c6502.ui :as ui])
  (:use-macros [c6502.macros :only [reverse-eval]]))

; Status Register bits
(def N 7) ; sign
(def V 6) ; overflow
(def B 4) ; brk
(def D 3) ; decimal
(def I 2) ; interrupt
(def Z 1) ; zero
(def C 0) ; carry

(defn reset-memory
  []
  (vec (repeat 65536 0)))


; cc = cycle count pseudo-register
(defrecord CPU
  [ac xr yr sp sr pc cc memory])

;
(defn to-byte
  "Ensure value fits in a byte"
  [x]
  (bit-and x 0xFF))


; Status Register handling

(defn set-zero
  [cpu reg]
  (if (zero? (reg cpu))
    (conj cpu {:sr (bit-set (:sr cpu) Z)})
    (conj cpu {:sr (bit-clear (:sr cpu) Z)})))

(defn set-sign
  [cpu reg]
  (if (bit-test (reg cpu) N)
    (conj cpu {:sr (bit-set (:sr cpu) N)})
    (conj cpu {:sr (bit-clear (:sr cpu) N)})))


(defn read-byte
  [cpu addr]
  (nth (:memory cpu) addr))

(defn read-little-endian
  [b1 b2]
  (+ (bit-shift-left b2 8) b1))


(defn write-byte
  [cpu addr byte]
  (conj cpu {:memory (assoc (:memory cpu) addr byte)}))


(defn read-word
  [cpu addr]
  (read-little-endian (read-byte cpu addr) (read-byte cpu (inc addr))))

; Addressing modes

(defn immediate
  [cpu]
  {:value (read-byte cpu (inc (:pc cpu)))
   :pc 2
   :cc 2 })

(defn zero-page
  ([cpu delta]
    {:value (read-byte cpu (read-byte cpu (+ delta (inc (:pc cpu)))))
     :pc 2
     :cc 2})
  ([cpu]
    (zero-page cpu 0)))

(defn zero-page-x
  [cpu]
  (merge-with + (zero-page cpu (:xr cpu)) {:cc 1}))


(defn absolute
  ([cpu delta]
    {:value (read-byte cpu (read-word cpu (inc (:pc cpu))))
     :pc 3
     :cc 4})
  ([cpu]
   (absolute cpu 0)))


; FIXME page boundary penalty
(defn absolute-x
  [cpu]
  (absolute cpu (:xr cpu)))

; FIXME page boundary penalty
(defn absolute-y
  [cpu]
  (absolute cpu (:yr cpu)))

(defn pre-indexed-indirect
  [cpu]
  {:value (read-byte cpu (to-byte (+ (:value (zero-page cpu)) (:xr cpu))))
   :pc 2
   :cc 6})

; FIXME page boundary penalty
(defn post-indexed-indirect
  [cpu]
  (let [address (read-little-endian (:value (zero-page cpu)) (:value (zero-page (merge-with + cpu {:pc 1}))))]
    {:value (read-byte cpu (+ address (:yr cpu)))
     :pc 2
     :cc 5}))


; OPCODES

(defmulti opcode (fn [cpu] (nth (:memory cpu) (:pc cpu))))

(defn LDA
  [cpu load]
  "LDA Implementation"
  (-> cpu
    (conj {:pc (+ (:pc cpu) (:pc load))
           :ac (:value load)
           :cc (+ (:cc cpu) (:cc load))})
    (set-zero :ac)
    (set-sign :ac)))

(defmethod opcode 0xA9 [cpu]
  "LDA Immediate"
  (LDA cpu (immediate cpu)))

(defmethod opcode 0xA5 [cpu]
  "LDA Zero Page"
  (LDA cpu (immediate cpu)))

(defmethod opcode 0xB5 [cpu]
  "LDA Zero Page X"
  (LDA cpu (zero-page-x cpu)))

(defmethod opcode 0xAD [cpu]
  "LDA Absolute"
  (LDA cpu (absolute cpu)))

(defmethod opcode 0xBD [cpu]
  "LDA Absolute X"
  (LDA cpu (absolute-x cpu)))

(defmethod opcode 0xB9 [cpu]
  "LDA Absolute Y"
  (LDA cpu (absolute-y cpu)))

(defmethod opcode 0xA1 [cpu]
  "LDA (Indirect X)"
  (LDA cpu (pre-indexed-indirect cpu)))

(defmethod opcode 0xB1 [cpu]
  "LDA (Indirect), Y"
  (LDA cpu (post-indexed-indirect cpu)))


(defn STA
  [cpu load]
  "STA Implementation"
  (conj cpu {:pc (+ (:pc cpu) (:pc load))
             :memory (write-byte cpu (:value load) (:ac cpu))
             :cc (+ (:cc cpu) (:cc load))}))

(defmethod opcode 0x85 [cpu]
  "STA Zero Page"
  (STA cpu (zero-page cpu)))

(defmethod opcode 0x95 [cpu]
  "STA Zero Page X"
  (STA cpu (zero-page-x cpu)))

(defmethod opcode 0x80 [cpu]
  "STA Absolute"
  (STA cpu (absolute cpu)))

(defmethod opcode 0x90 [cpu]
  "STA Absolute X"
  (STA cpu (absolute-x cpu)))

(defmethod opcode 0x99 [cpu]
  "STA Absolute Y"
  (STA cpu (absolute-y cpu)))

(defmethod opcode 0x81 [cpu]
  "STA (Indirect  X)"
  (STA cpu (pre-indexed-indirect cpu)))

(defmethod opcode 0x91 [cpu]
  "STA (Indirect) Y"
  (STA cpu (post-indexed-indirect cpu)))




; JMP Immediate
(defmethod opcode 0x4C
  [cpu]
  (conj cpu {:pc (+ (bit-shift-left (nth (:memory cpu) (inc (:pc cpu))) 8)
                    (nth (:memory cpu) (+ 2 (:pc cpu))))
             :cc (+ (:cc cpu) 3)}))

; INCX
(defmethod opcode 0xE8
  [cpu]
  (merge-with + cpu {:pc 1
                     :xr 1
                     :cc 2}))



(def running-cpu
  (atom (CPU. 0 1 2 3 4 0 0 (reset-memory))))

(defn step
  [cpu]
  (opcode cpu))


(defn main
  []
  (c6502.ui/render @running-cpu))


; tests

(def test-program
  [0xA9 0x02 0x85 0x05 0x00 0x00 0xE8 0x4C 0x00 0x00]) ; LDA 0x01, STA 0x00, INCX, JMP $0x0000

(def testCPU (atom (CPU. 0 1 0 0 0 0 0 test-program)))

(swap! testCPU step @testCPU)

(defn benchmark
  []
  (let [start (.getTime (js/Date.))]
    (doall
      (for [i (range 0 100000)]
      (swap! testCPU step @testCPU)))
    (js/console.log start)
    (js/console.log (.getTime (js/Date.)))
    (/ (:cc @testCPU) (/ (- (.getTime (js/Date.)) start) 1000))))




