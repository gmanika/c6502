(ns nes
  (:require [c6502]
            [ui]
            [goog.net.XhrIo]
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
  (reset! NESConsole (pop @history)))

(defn nes-loader
  [rom]
  (vec (concat (repeat 0xC000 0) (subvec rom 16))))

(defn receiver [event]
  (let [response (.-response (.-target event))]
    (compare-and-set! NESConsole @NESConsole (c6502/CPU. 0 0 0 0xFD 0x36 0xC000 0
                                                         (nes-loader (array-seq (js/Uint8Array. response)))))))

(defn rom-loader
  []
  (let [req (js/XMLHttpRequest.)]
    (aset req "onreadystatechange" receiver)
    (aset req "responseType" "arraybuffer")
    (.open req "GET" "http://zorked.net/nestest.nes")
    (.send req)))



(rom-loader)
@NESConsole
(c6502/step @NESConsole)
(swap! NESConsole c6502/step @NESConsole)
(undo)

(count @history)

(dissoc (swap! NESConsole c6502/step @NESConsole) :memory)


