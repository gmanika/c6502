(ns c6502
  (:require [ui])
  (:use-macros [c6502.macros :only [defopcodes]]))

; Documentation: http://nesdev.com/6502.txt

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
  [cpu value]
  (if (zero? value)
    (conj cpu {:sr (bit-set (:sr cpu) Z)})
    (conj cpu {:sr (bit-clear (:sr cpu) Z)})))

(defn set-sign
  [cpu value]
  (if (neg? value)
    (conj cpu {:sr (bit-set (:sr cpu) N)})
    (conj cpu {:sr (bit-clear (:sr cpu) N)})))

(defn set-overflow
  [cpu value]
  (if (> value 255)
    (conj cpu {:sr (bit-set (:sr cpu) V)})
    (conj cpu {:sr (bit-clear (:sr cpu) V)})))


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

(defn push-stack
  [cpu byte]
  (conj cpu {:memory (assoc (:memory cpu) (:sp cpu) byte)
             :sp (inc (:sp cpu))}))

(defn pull-stack
  [cpu reg]
  (conj cpu {reg (read-byte cpu (:sp cpu))
             :sp (dec (:sp cpu))}))

(defn pull-stack-word
  [cpu reg]
  (conj cpu {reg (read-word cpu (:sp cpu))
             :sp (- (:sp cpu) 2)}))



(defn bit-rotate-right
  [byte]
  (to-byte (bit-or (bit-shift-right byte 1) (bit-shift-left byte 7))))

(defn bit-rotate-left
  [byte]
  (to-byte (bit-or (bit-shift-left byte 1) (bit-shift-right byte 7))))

; Addressing modes

(defn implied
  [cpu]
  {:addr nil
   :pc 1
   :cc 2})

(defn immediate
  [cpu]
  {:addr (inc (:pc cpu))
   :pc 2
   :cc 2 })

(defn zero-page
  ([cpu delta]
    {:addr (read-byte cpu (+ delta (inc (:pc cpu))))
     :pc 2
     :cc 2})
  ([cpu]
    (zero-page cpu 0)))

(defn zero-page-x
  [cpu]
  (merge-with + (zero-page cpu (:xr cpu)) {:cc 1}))

(defn zero-page-y
  [cpu]
  (merge-with + (zero-page cpu (:yr cpu)) {:cc 1}))

(defn absolute
  ([cpu delta]
    {:addr (read-word cpu (inc (:pc cpu)))
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
  {:addr (to-byte (+ (:addr (zero-page cpu))  (:xr cpu)))
   :pc 2
   :cc 6})

; FIXME page boundary penalty
(defn post-indexed-indirect
  [cpu]
  {:addr (+ (read-word cpu (:addr (zero-page cpu))) (:yr cpu))
   :pc 2
   :cc 5})

(defn indirect
  [cpu]
  {:addr (read-word cpu (:addr (absolute cpu)))
   :pc 3
   :cc 5})


; OPCODES

(defmulti opcode (fn [cpu] (nth (:memory cpu) (:pc cpu))))


(defmethod opcode 0xE8 [cpu]
  "INCX"
  (merge-with + cpu {:pc 1
                     :xr 1
                     :cc 2}))


(defmethod opcode 0x18 [cpu]
  "CLC Implementation"
  (conj cpu {:pc (inc (:pc cpu))
             :sr (bit-clear (:sr cpu) C)}))

(defmethod opcode 0x58 [cpu]
  "CLI Implementation"
  (conj cpu {:pc (inc (:pc cpu))
             :sr (bit-clear (:sr cpu) I)}))

(defmethod opcode 0xB8 [cpu]
  "CLV Implementation"
  (-> cpu
      (conj {:pc (inc (:pc cpu))}
             :sr (bit-clear (:sr cpu) V))))

(defn CMP
  [cpu load]
  "CMP Implementation"
  (let [src (- (:ac cpu) (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src))))

(defopcodes CMP
  [[0xC9 immediate]
   [0xC5 zero-page]
   [0xD5 zero-page-x]
   [0xCD absolute]
   [0xDD absolute-x]
   [0xD9 absolute-y]
   [0xC1 pre-indexed-indirect]
   [0xD1 post-indexed-indirect]])

(defn CMPX
  [cpu load]
  "CMPX Implementation"
  (let [src (- (:xr cpu) (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src))))

(defopcodes CMPX
  [[0xE0 immediate]
   [0xE4 zero-page]
   [0xEC absolute]])

(defn CMPY
  [cpu load]
  "CMPX Implementation"
  (let [src (- (:yr cpu) (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src))))

(defopcodes CMPY
  [[0xC0 immediate]
   [0xC4 zero-page]
   [0xCC absolute]])

(defn DEC
  [cpu load]
  "DEC Implementation"
  (let [byte (dec (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :memory (:memory (write-byte cpu (:addr load) byte))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero byte)
        (set-sign byte))))

(defopcodes DEC
  [[0xC6 zero-page]
   [0xD6 zero-page-x]
   [0xCE absolute]
   [0xDE absolute-x]])

(defmethod opcode 0xCA [cpu]
  "DEX Implementation"
  (let [src (dec (:xr cpu))]
    (-> cpu
      (conj {:pc (inc (:pc cpu))
             :xr src}
            (:cc (+ (:cc cpu) 2)))
      (set-zero src)
      (set-sign src))))

(defmethod opcode 0x88 [cpu]
  "DEY Implementation"
  (let [src (dec (:yr cpu))]
    (-> cpu
      (conj {:pc (inc (:pc cpu))
             :yr src}
            (:cc (+ (:cc cpu) 2)))
      (set-zero src)
      (set-sign src))))

(defn EOR
  [cpu load]
  "EOR Implementation"
  (let [src (bit-xor (:ac cpu)(read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :ac src
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src))))

(defopcodes EOR
  [[0x49 immediate]
   [0x45 zero-page]
   [0x55 zero-page-x]
   [0x40 absolute]
   [0x50 absolute-x]
   [0x59 absolute-y]
   [0x41 pre-indexed-indirect]
   [0x51 post-indexed-indirect]])

(defn INC
  [cpu load]
  "INC Implementation"
  (let [src (inc (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :memory (:memory (write-byte cpu (:addr load) src))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src))))

(defopcodes INC
  [[0xE6 zero-page]
   [0xF6 zero-page-x]
   [0xEE absolute]
   [0xFE absolute-x]])

(defmethod opcode 0xE8 [cpu]
  "INX"
  (let [src (inc (:xr cpu))]
    (-> cpu
      (conj {:pc (inc (:pc cpu))
             :xr src
             :cc (+ (:cc cpu) 2)})
      (set-zero src)
      (set-sign src))))

(defmethod opcode 0xC8 [cpu]
  "INY"
  (let [src (inc (:yr cpu))]
    (-> cpu
      (conj {:pc (inc (:pc cpu))
             :yr src
             :cc (+ (:cc cpu) 2)})
      (set-zero src)
      (set-sign src))))

(defn JMP
  [cpu load]
  "JMP Implementation"
  (conj cpu {:pc (:addr load)
             :cc (+ (:cc cpu) (:cc load))}))

(defopcodes JMP
  [[0x4C absolute]
   [0x6C indirect]])

(defn JSR
  [cpu load]
  "JSR Implementation"
  (let [return (+ (:pc cpu) (:pc load))]
    (-> cpu
        (conj {:pc (:addr load)
               :cc (+ (:cc cpu) 2)})
        (push-stack (to-byte return))
        (push-stack (to-byte (bit-shift-right return 8))))))

(defopcodes JSR
  [[0x20 absolute]])

(defn LDA
  [cpu load]
  "LDA Implementation"
  (-> cpu
    (conj {:pc (+ (:pc cpu) (:pc load))
           :ac (read-byte cpu (:addr load))
           :cc (+ (:cc cpu) (:cc load))})
    (set-zero (:ac cpu))
    (set-sign (:ac cpu))))

(defopcodes LDA
  [[0xA9 immediate]
   [0xA5 zero-page]
   [0xB5 zero-page-x]
   [0xAD absolute]
   [0xBD absolute-x]
   [0xB9 absolute-y]
   [0xA1 pre-indexed-indirect]
   [0xB1 post-indexed-indirect]])


(defn LDX
  [cpu load]
  "LDX Implementation"
  (-> cpu
    (conj {:pc (+ (:pc cpu) (:pc load))
           :xr (read-byte cpu (:addr load))
           :cc (+ (:cc cpu) (:cc load))})
    (set-zero (:xr cpu))
    (set-sign (:xr cpu))))

(defopcodes LDX
  [[0xA2 immediate]
   [0xA6 zero-page]
   [0xB6 zero-page-y]
   [0xAE absolute]
   [0xBE absolute-y]])

(defn LDY
  [cpu load]
  "LDY Implementation"
  (-> cpu
    (conj {:pc (+ (:pc cpu) (:pc load))
           :yr (read-byte cpu (:addr load))
           :cc (+ (:cc cpu) (:cc load))})
    (set-zero (:yr cpu))
    (set-sign (:yr cpu))))

(defopcodes LDY
  [[0xA0 immediate]
   [0xA4 zero-page]
   [0xB4 zero-page-x]
   [0xAC absolute]
   [0xBC absolute-x]])

(defn LSR
  [cpu load]
  "LSR Implementation"
  (let [src (bit-shift-right (:addr load) 1)]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) src))
             :cc (+ (:cc cpu) (:cc load))})
      (set-zero src)
      (set-sign 0))))

(defopcodes LSR
  [[0x46 zero-page]
   [0x56 zero-page-x]
   [0x4E absolute]
   [0x5E absolute-x]])

(defmethod opcode 0x4A [cpu]
  "LSR A Implementation"
  (let [src (bit-shift-right (:ac cpu) 1)]
    (-> cpu
        (conj {:pc (+ (:pc cpu) 1)}
              {:ac src}
              {:cc (+ (:cc cpu) 2)})
        (set-zero src)
        (set-sign 0))))

(defmethod opcode 0xEA [cpu]
  "NOP Implementation"
  (conj cpu {:pc (inc (:pc cpu))
             :cc (+ (:cc cpu) 2)}))

(defn ORA
  [cpu load]
  "ORA Implementation"
  (let [src (bit-or (:ac cpu) (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :ac src
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src))))

(defopcodes ORA
  [[0x09 immediate]
   [0x05 zero-page]
   [0x15 zero-page-x]
   [0x0D absolute]
   [0x1D absolute-x]
   [0x19 absolute-y]
   [0x01 pre-indexed-indirect]
   [0x11 post-indexed-indirect]])

(defmethod opcode 0x48 [cpu]
  "PHA Implementation"
  (-> cpu
      (conj {:pc (inc (:pc cpu))
             :cc (+ (:cc cpu) 3)})
      (push-stack (:ac cpu))))

(defmethod opcode 0x08 [cpu]
  "PHP Implementation"
  (-> cpu
      (conj {:pc (inc (:pc cpu))
             :cc (+ (:cc cpu) 3)})
      (push-stack (:sr cpu))))

(defmethod opcode 0x68 [cpu]
  "PLA Implementation"
  (-> cpu
      (conj cpu {:pc (inc (:pc cpu))
                 :cc (+ (:cc cpu) 4)})
      (pull-stack :ac)))

(defmethod opcode 0x68 [cpu]
  "PLA Implementation"
  (-> cpu
      (conj cpu {:pc (inc (:pc cpu))
                 :cc (+ (:cc cpu) 4)})
      (pull-stack :sp)))



(defn ROL
  [cpu load]
  "ROL Implementation"
  (let [src (bit-rotate-left (:addr load))]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) src))
             :cc (+ (:cc cpu) (:cc load))})
      (set-zero src)
      (set-sign 0))))

(defopcodes ROL
  [[0x26 zero-page]
   [0x36 zero-page-x]
   [0x2E absolute]
   [0x3E absolute-x]])

(defmethod opcode 0x2A [cpu]
  "ROL A Implementation"
  (let [src (bit-rotate-left (:ac cpu))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) 1)}
              {:ac src}
              {:cc (+ (:cc cpu) 2)})
        (set-zero src)
        (set-sign 0))))

(defn ROR
  [cpu load]
  "ROR Implementation"
  (let [src (bit-rotate-right (:addr load))]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) src))
             :cc (+ (:cc cpu) (:cc load))})
      (set-zero src)
      (set-sign 0))))

(defopcodes ROR
  [[0x66 zero-page]
   [0x76 zero-page-x]
   [0x6E absolute]
   [0x7E absolute-x]])

(defmethod opcode 0x6A [cpu]
  "ROR A Implementation"
  (let [src (bit-rotate-right (:ac cpu))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) 1)}
              {:ac src}
              {:cc (+ (:cc cpu) 2)})
        (set-zero src)
        (set-sign 0))))


(defmethod opcode 0x60 [cpu]
  "RTS Implementation"
  (-> cpu
      (conj {:cc (+ (:cc cpu) 6)})
      (pull-stack :sr)
      (pull-stack-word :pc)))

(defmethod opcode 0x38 [cpu]
  "SEC Implementation"
  (conj cpu {:cc (+ (:cc cpu) 2)
             :sr (bit-set (:sr cpu) C)
             :pc (inc (:pc cpu))}))

(defn STA
  [cpu load]
  "STA Implementation"
  (conj cpu {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) (:ac cpu)))
             :cc (+ (:cc cpu) (:cc load))}))

(defopcodes STA
  [[0x85 zero-page]
   [0x95 zero-page-x]
   [0x8D absolute]
   [0x9D absolute-x]
   [0x99 absolute-y]
   [0x81 pre-indexed-indirect]
   [0x91 post-indexed-indirect]])


(defn STX
  [cpu load]
  "STX Implementation"
  (conj cpu {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) (:xr cpu)))
             :cc (+ (:cc cpu) (:cc load))}))

(defopcodes STX
  [[0x86 zero-page]
   [0x96 zero-page-y]
   [0x8E absolute]])

(defn STY
  [cpu load]
  "STY Implementation"
  (conj cpu {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) (:yr cpu)))
             :cc (+ (:cc cpu) (:cc load))}))

(defopcodes STY
  [[0x84 zero-page]
   [0x94 zero-page-x]
   [0x8C absolute]])

(defn move
  "Implements all register -> register moves"
  [cpu source destination]
  (-> cpu
      (conj {:pc (inc (:pc cpu))
             destination (source cpu)}
             :cc (+ (:cc cpu) 2))))

(defmethod opcode 0xAA [cpu]
  "TAX Implementation"
  (move cpu :ac :xr))

(defmethod opcode 0xA8 [cpu]
  "TAY Implementation"
  (move cpu :ac :yr))

(defmethod opcode 0xBA [cpu]
  "TSX Implementation"
  (move cpu :xr :sp))

(defmethod opcode 0x8A [cpu]
  "TXA Implementation"
  (move cpu :xr :ac))

(defmethod opcode 0x9A [cpu]
  "TXS Implementation"
  (move cpu :xr :sp))

(defmethod opcode 0x9A [cpu]
  "TYA Implementation"
  (move cpu :yr :ac))

(defn branch
  "Implements branch instructions"
  [cpu diff]
  (if (< diff 128)
    (conj cpu {:pc (+ (:pc cpu) diff 2)
               :cc (+ (:cc cpu) 3)})
    (conj cpu {:pc (+ (:pc cpu) (- (bit-clear diff 7)) 2)
               :cc (+ (:cc cpu) 3)})))

(defmethod opcode 0x90 [cpu]
  "BCC Implementation"
  (if (not (bit-test (:sr cpu) C))
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))


(defmethod opcode 0xB0 [cpu]
  "BCS Implementation"
  (if (bit-test (:sr cpu) C)
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))

(defmethod opcode 0xF0 [cpu]
  "BEQ Implementation"
  (if (bit-test (:sr cpu) Z)
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))

(defmethod opcode 0xD0 [cpu]
  "BNE Implementation"
  (if (not (bit-test (:sr cpu) Z))
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))

(defn BIT
  [cpu load]
  "BIT Implementation"
  (let [src (read-byte cpu (:addr load))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero (- src (:ac cpu)))
        (set-sign src)
        (set-overflow src))))



(defopcodes BIT
  [[0x24 zero-page]
   [0x2C absolute]])





(def running-cpu
  (atom (CPU. 0 1 2 3 4 0 0 (reset-memory))))

(defn step
  [cpu]
  (opcode cpu))



;(defn main
;  []
;  (c6502.ui/render @running-cpu))


; tests

(def test-program
  [0xA8 0x004 0x02 0x00 0x00 0x00 0x12 0x00 0x00])

(def testCPU (atom (CPU. 1 5 0 5 0 0 0 test-program)))

(swap! testCPU step @testCPU)
(def STAtest (CPU. 2 1 0 6 0 0 0 test-program))

(immediate STAtest)
(post-indexed-indirect STAtest)
STAtest
(step STAtest)

(step (step STAtest))
(zero-page STAtest)

(defn benchmark
  []
  (let [start (.getTime (js/Date.))]
    (doall
      (for [i (range 0 100000)]
      (swap! testCPU step @testCPU)))
    (js/console.log start)
    (js/console.log (.getTime (js/Date.)))
    (/ (:cc @testCPU) (/ (- (.getTime (js/Date.)) start) 1000))))

(c6502/opcode {:memory [0xE6 0 0 0 0]})
