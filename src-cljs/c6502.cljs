(ns c6502
  (:require [c6502.ui :as ui]))

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

; OPCODES

(defmulti opcode (fn [cpu] (nth (:memory cpu) (:pc cpu))))

; LDA Immediate
(defmethod opcode 0xA9
  [cpu]
  (-> cpu
      (conj {:pc (+ (:pc cpu) 2)
             :ac (nth (:memory cpu) (inc (:pc cpu)))
             :cc (+ (:cc cpu) 2)})
      (set-zero :ac)
      (set-sign :ac)))

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
  [0xA9 0x10 0xE8 0x4C 0x00 0x00]) ; LDA 0x10, INCX, JMP $0x0000

(def testCPU (atom (CPU. 0 0 0 0 0 0 0 test-program)))

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




