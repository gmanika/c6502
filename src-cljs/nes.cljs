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


(rom-loader)

;; tests

(defn run!
  []
  (dotimes [n (count tests/nestestlog)]
    (swap! NESConsole c6502/step @NESConsole)))

(defn runtest
  []
  (run!)
  (when (= (count @history) 5004)
    (js/console.log "All tests before illegal opcodes pass!")))

(defn comparetests
  []
  (map show-state
    (first
      (filter (fn [[a b]] (not (= (simplify a) (simplify b))))
        (map vector @history tests/nestestlog)))))
