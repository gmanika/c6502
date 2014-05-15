(ns nes
  (:require [c6502]
            [ui]
            [goog.net.XhrIo]
            [goog.net.EventType :as eventtype]))

(def nesROM (atom nil))

(defn nes-loader
  [rom]
  (vec (concat (repeat 0xC000 0) (subvec rom 16))))



(defn receiver [event]
  (let [response (.-response (.-target event))]
    (compare-and-set! nesROM @nesROM (array-seq (js/Uint8Array. response)))))


(defn rom-loader
  []
  (let [req (js/XMLHttpRequest.)]
    (aset req "onreadystatechange" receiver)
    (aset req "responseType" "arraybuffer")
    (.open req "GET" "http://zorked.net/nestest.nes")
    (.send req)))


(rom-loader)
@nesROM
(subvec @nesROM 16)

(def NESConsole (atom (c6502/CPU. 0 0 0 0xFD 0x36 0xC000 0 (nes-loader @nesROM))))


(:pc @NESConsole)
(count (:memory @NESConsole))

(subvec (:memory @NESConsole) (:pc @NESConsole))

(c6502/read-byte NESConsole 0xC00)

(c6502/read-byte NESConsole 3072)

(ui/render NESConsole)

(js/console.log (dissoc @NESConsole :memory))
(c6502/step @NESConsole)

(dissoc (swap! NESConsole c6502/step @NESConsole) :memory)


(c6502/step {:memory [0xE6 0 0 0 0]})

(nth (:memory @NESConsole) (:pc @NESConsole))
@nesROM
(nth (:memory NESConsole) 0xC000)

