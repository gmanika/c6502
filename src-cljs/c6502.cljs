(ns c6502
  (:require [ui])
  (:use-macros [c6502.macros :only [defopcodes]]))

; Documentation: http://nesdev.com/6502.txt (often wrong)
;                http://www.6502.org/tutorials/6502opcodes.html

; Status Register bits
(def N 7) ; sign
(def V 6) ; overflow
(def G 5) ; "ghost"
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

(defn set-zero-register
  [cpu reg]
  (set-zero cpu (reg cpu)))

(defn set-sign
  [cpu value]
  (if (bit-test value 7)
    (conj cpu {:sr (bit-set (:sr cpu) N)})
    (conj cpu {:sr (bit-clear (:sr cpu) N)})))

(defn set-sign-register
  [cpu reg]
  (set-sign cpu (reg cpu)))

(defn set-overflow
  [cpu value]
  (if (bit-test value V)
    (conj cpu {:sr (bit-set (:sr cpu) V)})
    (conj cpu {:sr (bit-clear (:sr cpu) V)})))

(defn set-overflow-adc
  [cpu src result]
  (if (or (= (bit-test result 7) (bit-test (:ac cpu) 7))
          (= (bit-test result 7) (bit-test src 7)))
    (conj cpu {:sr (bit-clear (:sr cpu) V)})
    (conj cpu {:sr (bit-set (:sr cpu) V)})))

(defn set-overflow-sbc
  [cpu src result]
  (set-overflow-adc cpu (bit-flip src 7) result))


(set-overflow-adc {:ac 127 :sr 1} 127 255)
(set-overflow-adc {:ac 0 :sr 0} 0x69 0x69)

(bit-test (bit-xor 0x7f 127) 7)

(bit-test (bit-xor 0x7F 0x7F) 7)

(defn set-carry
  [cpu value]
  (if (> value 255)
    (conj cpu {:sr (bit-set (:sr cpu) C)})
    (conj cpu {:sr (bit-clear (:sr cpu) C)})))

(defn set-carry-cmp
  [cpu value]
  (if (< value 0)
    (conj cpu {:sr (bit-clear (:sr cpu) C)})
    (conj cpu {:sr (bit-set (:sr cpu) C)})))

(defn handle-plp
  "Kludge to handle bits 4 and 5"
  [cpu]
  (conj cpu {:sr (bit-set (bit-clear (:sr cpu) B) G)}))

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
  (conj cpu {:memory (assoc (:memory cpu) (+ 0x100 (:sp cpu)) byte)
             :sp (dec (:sp cpu))}))

(defn pull-stack
  [cpu reg]
  (conj cpu {reg (read-byte cpu (+ 0x100 (inc (:sp cpu))))
             :sp (inc (:sp cpu))}))

(defn pull-stack-pc
  [cpu]
  (conj cpu {:pc (inc (read-word cpu (+ 0x100 (inc (:sp cpu)))))
             :sp (+ (:sp cpu) 2)}))

(defn pull-stack-pc-noinc
  [cpu]
  (conj cpu {:pc (read-word cpu (+ 0x100 (inc (:sp cpu))))
             :sp (+ (:sp cpu) 2)}))



(defn bit-rotate-right
  [byte]
  (to-byte (bit-or (bit-shift-right byte 1) (bit-shift-left byte 7))))


(defn bit-rotate-left
  [byte]
  (to-byte (bit-or (bit-shift-left byte 1) (bit-shift-right byte 8))))

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
    {:addr (read-byte cpu (to-byte (+ delta (read-byte cpu (inc (:pc cpu))))))
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
    {:addr (bit-and (+ (read-word cpu (inc (:pc cpu))) delta) 0xFFFF)
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
  (let [iaddr (+ (read-byte cpu (inc (:pc cpu))) (:xr cpu))]
    (js/console.log (:pc cpu) iaddr)
    {:addr (+ (read-byte cpu (to-byte iaddr)) (bit-shift-left (read-byte cpu (to-byte (inc iaddr))) 8))
     :pc 2
     :cc 6}))

; FIXME page boundary penalty
(defn post-indexed-indirect
  [cpu]
  (let [addr (read-byte cpu (inc (:pc cpu)))
        iaddr (+ (read-byte cpu (to-byte addr)) (bit-shift-left (read-byte cpu (to-byte (inc addr))) 8))]
    {:addr (bit-and (+ iaddr (:yr cpu)) 0xFFFF)
     :pc 2
     :cc 5}))

(defn indirect
  [cpu]
  (let [addr (read-word cpu (inc (:pc cpu)))]
    ; 6502 bug
    (if (= 0xFF (bit-and addr 0xFF))
      {:addr (+ (read-byte cpu addr) (bit-shift-left (read-byte cpu (bit-and 0xFF00 addr)) 8))
       :pc 3
       :cc 5 }
      {:addr (read-word cpu (read-word cpu (inc (:pc cpu))))
       :pc 3
       :cc 5})))


; OPCODES

(defmulti opcode (fn [cpu] (nth (:memory cpu) (:pc cpu))))

(defn ADC
  [cpu load]
  "ADC Implementation"
  (let [src (read-byte cpu (:addr load))
        result (+ src (:ac cpu) (if (bit-test (:sr cpu) C) 1 0))]
    (-> cpu
        (set-overflow-adc src result)
        (conj {:pc (+ (:pc cpu) (:pc load))
               :ac (to-byte result)
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero (to-byte result))
        (set-sign (to-byte result))
        (set-carry result))))

(defopcodes ADC
  [[0x69 immediate]
   [0x65 zero-page]
   [0x75 zero-page-x]
   [0x6D absolute]
   [0x7D absolute-x]
   [0x79 absolute-y]
   [0x61 pre-indexed-indirect]
   [0x71 post-indexed-indirect]])

(defn SBC
  [cpu load]
  "SBC Implementation"
  (let [src (read-byte cpu (:addr load))
        result (- (:ac cpu) src (if (bit-test (:sr cpu) C) 0 1))]
    (-> cpu
        (set-overflow-sbc src result)
        (conj {:pc (+ (:pc cpu) (:pc load))
               :ac (to-byte result)
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero (to-byte result))
        (set-sign (to-byte result))
        (set-carry-cmp result))))

(defopcodes SBC
  [[0xE9 immediate]
   [0xE5 zero-page]
   [0xF5 zero-page-x]
   [0xED absolute]
   [0xFD absolute-x]
   [0xF9 absolute-y]
   [0xE1 pre-indexed-indirect]
   [0xF1 post-indexed-indirect]])



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
  (conj cpu {:pc (inc (:pc cpu))
             :sr (bit-clear (:sr cpu) V)}))

(defn CMP
  [cpu load]
  "CMP Implementation"
  (let [src (- (:ac cpu) (read-byte cpu (:addr load)))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :cc (+ (:cc cpu) (:cc load))})
        (set-zero src)
        (set-sign src)
        (set-carry-cmp src))))


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
        (set-sign src)
        (set-carry-cmp src))))

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
        (set-sign src)
        (set-carry-cmp src))))

(defopcodes CMPY
  [[0xC0 immediate]
   [0xC4 zero-page]
   [0xCC absolute]])

(defn DEC
  [cpu load]
  "DEC Implementation"
  (let [byte (to-byte (dec (read-byte cpu (:addr load))))]
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
  (-> cpu
    (conj {:pc (inc (:pc cpu))
           :xr (to-byte (dec (:xr cpu)))
           :cc (+ (:cc cpu) 2)})
    (set-zero-register :xr)
    (set-sign-register :xr)))

(defmethod opcode 0x88 [cpu]
  "DEY Implementation"
  (-> cpu
    (conj {:pc (inc (:pc cpu))
           :yr (to-byte (dec (:yr cpu)))
           :cc (+ (:cc cpu) 2)})
    (set-zero-register :yr)
    (set-sign-register :yr)))

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
   [0x4D absolute]
   [0x5D absolute-x]
   [0x59 absolute-y]
   [0x41 pre-indexed-indirect]
   [0x51 post-indexed-indirect]])

(defn INC
  [cpu load]
  "INC Implementation"
  (let [src (to-byte (inc (read-byte cpu (:addr load))))]
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
  (-> cpu
    (conj {:pc (inc (:pc cpu))
           :xr (to-byte (inc (:xr cpu)))
           :cc (+ (:cc cpu) 2)})
    (set-zero-register :xr)
    (set-sign-register :xr)))

(defmethod opcode 0xC8 [cpu]
  "INY"
  (-> cpu
    (conj {:pc (inc (:pc cpu))
           :yr (to-byte (inc (:yr cpu)))
           :cc (+ (:cc cpu) 2)})
    (set-zero-register :yr)
    (set-sign-register :yr)))

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
  (let [return (+ (:pc cpu) (:pc load) -1)]
    (-> cpu
        (conj {:pc (:addr load)
               :cc (+ (:cc cpu) 2)})
        (push-stack (to-byte (bit-shift-right return 8)))
        (push-stack (to-byte return)))))

(defopcodes JSR
  [[0x20 absolute]])

(defn LDA
  [cpu load]
  "LDA Implementation"
  (let [src (read-byte cpu (:addr load))]
    (js/console.log "LDA" (:addr load) (read-byte cpu (:addr load)))
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :ac src
             :cc (+ (:cc cpu) (:cc load))})
      (set-zero src)
      (set-sign src))))

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
  (let [src (read-byte cpu (:addr load))]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :xr src
             :cc (+ (:cc cpu) (:cc load))})
      (set-zero src)
      (set-sign src))))

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
    (set-zero-register :yr)
    (set-sign-register :yr)))

(defopcodes LDY
  [[0xA0 immediate]
   [0xA4 zero-page]
   [0xB4 zero-page-x]
   [0xAC absolute]
   [0xBC absolute-x]])

(defn AND
  [cpu load]
  "AND Implementation"
  (let [src (read-byte cpu (:addr load))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) (:pc load))
               :ac (bit-and (:ac cpu) src)
               :cc (+ (:cc cpu) (:cc load))})
        (set-sign-register :ac)
        (set-zero-register :ac))))

(defopcodes AND
  [[0x29 immediate]
   [0x25 zero-page]
   [0x35 zero-page-x]
   [0x2D absolute]
   [0x3D absolute-x]
   [0x39 absolute-y]
   [0x21 pre-indexed-indirect]
   [0x31 post-indexed-indirect]])

(defn ASL
  [cpu load]
  "ASL Implementation"
  (let [shifted (bit-shift-left (read-byte cpu (:addr load)) 1)]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) (to-byte shifted)))
             :cc (+ (:cc cpu) (:cc load))})
      (set-carry shifted)
      (set-zero (to-byte shifted))
      (set-sign shifted))))

(defopcodes ASL
  [[0x06 zero-page]
   [0x16 zero-page-x]
   [0x0E absolute]
   [0x1E absolute-x]])

(defmethod opcode 0x0A [cpu]
  "ASL A Implementation"
  (let [src (bit-shift-left (:ac cpu) 1)]
    (-> cpu
      (conj {:pc (inc (:pc cpu))
             :ac (to-byte src)
             :cc (+ (:cc cpu) 2)})
      (set-carry src)
      (set-zero (to-byte src))
      (set-sign src))))

(defn LSR
  [cpu load]
  "LSR Implementation"
  (let [src (read-byte cpu (:addr load))
        rotated (bit-shift-right src 1)]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) rotated))
             :cc (+ (:cc cpu) (:cc load))})
      (set-carry (bit-and 0x100 (bit-shift-left src 8)))
      (set-zero rotated)
      (set-sign rotated))))

(defopcodes LSR
  [[0x46 zero-page]
   [0x56 zero-page-x]
   [0x4E absolute]
   [0x5E absolute-x]])

(defmethod opcode 0x4A [cpu]
  "LSR A Implementation"
  (let [src (:ac cpu)
        rotated (bit-shift-right src 1)]
    (-> cpu
        (conj {:pc (+ (:pc cpu) 1)}
              {:ac rotated}
              {:cc (+ (:cc cpu) 2)})
        (set-carry (bit-and 0x100 (bit-shift-left src 8)))
        (set-zero-register :ac)
        (set-sign-register :ac))))

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
      ; B bit behavior: http://visual6502.org/wiki/index.php?title=6502_BRK_and_B_bit
      (push-stack (bit-set (:sr cpu) B))))

(defmethod opcode 0x68 [cpu]
  "PLA Implementation"
  (-> cpu
      (conj cpu {:pc (inc (:pc cpu))
                 :cc (+ (:cc cpu) 4)})
      (pull-stack :ac)
      (set-zero-register :ac)
      (set-sign-register :ac)))

(defmethod opcode 0x28 [cpu]
  "PLP Implementation"
  (-> cpu
      (conj cpu {:pc (inc (:pc cpu))
                 :cc (+ (:cc cpu) 4)})
      (pull-stack :sr)
      (handle-plp)))


(defn ROL
  [cpu load]
  "ROL Implementation"
  (let [src (read-byte cpu (:addr load))
        rotated (bit-or (if (bit-test (:sr cpu) C) 0x01 0)
                        (bit-shift-left src 1))]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) (to-byte rotated)))
             :cc (+ (:cc cpu) (:cc load))})
        (set-carry (bit-and rotated 0x100))
        (set-zero (to-byte rotated))
        (set-sign (to-byte rotated)))))

(defopcodes ROL
  [[0x26 zero-page]
   [0x36 zero-page-x]
   [0x2E absolute]
   [0x3E absolute-x]])

(defmethod opcode 0x2A [cpu]
  "ROL A Implementation"
  (let [src (:ac cpu)
        rotated (bit-or (if (bit-test (:sr cpu) C) 0x01 0)
                        (bit-shift-left src 1))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) 1)}
              {:ac (to-byte rotated)}
              {:cc (+ (:cc cpu) 2)})
        (set-carry (bit-and rotated 0x100))
        (set-zero (to-byte rotated))
        (set-sign (to-byte rotated)))))


(defn ROR
  [cpu load]
  "ROR Implementation"
  (let [src (read-byte cpu (:addr load))
        rotated (bit-or (if (bit-test (:sr cpu) C) 0x80 0)
                         (bit-shift-right src 1))]
    (-> cpu
      (conj {:pc (+ (:pc cpu) (:pc load))
             :memory (:memory (write-byte cpu (:addr load) rotated))
             :cc (+ (:cc cpu) (:cc load))})
      (set-carry (bit-and 0x100 (bit-shift-left src 8)))
      (set-zero rotated)
      (set-sign rotated))))

(defopcodes ROR
  [[0x66 zero-page]
   [0x76 zero-page-x]
   [0x6E absolute]
   [0x7E absolute-x]])

(defmethod opcode 0x6A [cpu]
  "ROR A Implementation"
  (let [src (:ac cpu)
        rotated (bit-or (if (bit-test (:sr cpu) C) 0x80 0)
                         (bit-shift-right src 1))]
    (-> cpu
        (conj {:pc (+ (:pc cpu) 1)}
              {:ac rotated}
              {:cc (+ (:cc cpu) 2)})
        (set-carry (bit-and 0x100 (bit-shift-left src 8)))
        (set-zero rotated)
        (set-sign rotated))))


(defmethod opcode 0x60 [cpu]
  "RTS Implementation"
  (-> cpu
      (conj {:cc (+ (:cc cpu) 6)})
      (pull-stack-pc)))

(defmethod opcode 0x40 [cpu]
  "RTI Implementation"
  (-> cpu
      (conj {:cc (+ (:cc cpu) 6)})
      (pull-stack :sr)
      (pull-stack-pc-noinc)
      (handle-plp)))

(defmethod opcode 0x38 [cpu]
  "SEC Implementation"
  (conj cpu {:cc (+ (:cc cpu) 2)
             :sr (bit-set (:sr cpu) C)
             :pc (inc (:pc cpu))}))

(defmethod opcode 0x78 [cpu]
  "SEC Implementation"
  (conj cpu {:cc (+ (:cc cpu) 2)
             :sr (bit-set (:sr cpu) I)
             :pc (inc (:pc cpu))}))

(defmethod opcode 0xF8 [cpu]
  "SEI Implementation"
  (conj cpu {:cc (inc (:cc cpu))
             :sr (bit-set (:sr cpu) D)
             :pc (inc (:pc cpu))}))

(defmethod opcode 0xD8 [cpu]
  "CLD Implementation"
  (conj cpu {:cc (inc (:cc cpu))
             :sr (bit-clear (:sr cpu) D)
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
    (conj cpu {:pc (inc (:pc cpu))
               destination (source cpu)
               :cc (+ (:cc cpu) 2)})
    (set-zero-register destination)
    (set-sign-register destination)))

(defmethod opcode 0xAA [cpu]
  "TAX Implementation"
  (move cpu :ac :xr))

(defmethod opcode 0xA8 [cpu]
  "TAY Implementation"
  (move cpu :ac :yr))

(defmethod opcode 0xBA [cpu]
  "TSX Implementation"
  (move cpu :sp :xr))

(defmethod opcode 0x8A [cpu]
  "TXA Implementation"
  (move cpu :xr :ac))

(defmethod opcode 0x9A [cpu]
  "TXS Implementation"
  (-> cpu
    (conj cpu {:pc (inc (:pc cpu))
               :sp (:xr cpu)
               :cc (+ (:cc cpu) 2)})))

(defmethod opcode 0x98 [cpu]
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

(defmethod opcode 0x50 [cpu]
  "BVC Implementation"
  (if (not (bit-test (:sr cpu) V))
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))

(defmethod opcode 0x70 [cpu]
  "BVS Implementation"
  (if (bit-test (:sr cpu) V)
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))

(defmethod opcode 0x30 [cpu]
  "BMI Implementation"
  (if (bit-test (:sr cpu) N)
    (branch cpu (read-byte cpu (inc (:pc cpu))))
    (conj cpu {:pc (+ (:pc cpu) 2)
               :cc (+ (:cc cpu) 2)})))

(defmethod opcode 0x10 [cpu]
  "BPL Implementation"
  (if (not (bit-test (:sr cpu) N))
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
        (set-zero (bit-and src (:ac cpu)))
        (set-sign src)
        (set-overflow src))))

(defopcodes BIT
  [[0x24 zero-page]
   [0x2C absolute]])

(defn step
  [cpu]
  (opcode cpu))

