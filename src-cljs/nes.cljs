(ns nes
  (:require [c6502]
            [ui]
            [goog.net.XhrIo]
            [tests]
            [goog.net.EventType :as eventtype]))

(def NESConsole (atom nil))
(def history (atom []))


(add-watch NESConsole :history
           (fn [_ _ _ n]
             (when-not (= (last @history) n)
               (swap! history conj n))))


(defn undo
  []
  (swap! history pop)
  (reset! NESConsole (peek @history)))

(defn nes-loader
  [rom]
  (vec (concat (repeat 0xC000 0) (subvec rom 16))))

(defn receiver [event]
  (let [response (.-response (.-target event))]
    (compare-and-set! NESConsole @NESConsole (c6502/CPU. 0 0 0 0xFD 0x24 0xC000 0
                                                         (nes-loader (array-seq (js/Uint8Array. response)))))))

(defn rom-loader
  []
  (let [req (js/XMLHttpRequest.)]
    (aset req "onreadystatechange" receiver)
    (aset req "responseType" "arraybuffer")
    (.open req "GET" "http://zorked.net/nestest.nes")
    (.send req)))

(defn show-state
  [cpu]
  (map (fn [[k v]] [k (ui/hex v)]) (dissoc cpu :memory)))

(defn simplify
  [m]
  {:pc (:pc m) :sr (:sr m) :ac (:ac m) :xr (:xr m) :yr (:yr m) :sp (:sp m)})


;; repl

(rom-loader)


(defn run!
  []
  (dotimes [n (count tests/nestestlog)]
    (swap! NESConsole c6502/step @NESConsole)))
(run!)




(last @history)

(map show-state @history)

(show-state @NESConsole)
(show-state (swap! NESConsole c6502/step @NESConsole))

(count @history)
(count tests/nestestlog)
(show-state (last @history))

(c6502/step (last @history))
(show-state  (last @history))

(map show-state (take-while #(not (zero? (:pc %))) @history))
(map simplify (subvec tests/nestestlog 0 (count @history)))

(map show-state
     (first
      (filter (fn [[a b]] (not (= (simplify a) (simplify b))))
        (map vector @history tests/nestestlog))))


;(subvec (:memory z) 0 513)



; SR =A4
; SR = E4

(map (comp ui/hex :pc) @history)
(map show-state (take-last 5 (filter #(< (:cc %) 0x7F0) @history)))



; 26 = 0010 0110
; A4 = 1010 0110  Z off N on





