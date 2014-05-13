(ns nes
  (:require [c6502]
            [goog.net.XhrIo :as xhr]))

(def nesROM (atom nil))

(defn nes-loader
  [rom]
  (vec (concat
         (repeat 0x8000 0) rom (repeat (- 0xFFFF (count rom) 0x8000) 0))))



(defn receiver [event]
  (let [response (.-target event)]
    (js/console.log (.getResponseText response))
    (compare-and-set! nesROM @nesROM (vec (map (fn [x] (.charCodeAt x 0)) (.getResponseText response))))))

(defn rom-loader
  []
  (goog.net.XhrIo/send "http://zorked.net/nestest.nes" receiver "GET"))

(rom-loader)

(def NESConsole (c6502/CPU. 0 0 0 0 0x1000 0xC000 0 (nes-loader @nesROM)))
(c6502/step NESConsole)

@nesROM
(nth (:memory NESConsole) (+ 0(:pc NESConsole)))

(filter #(not (zero? %)) (:memory NESConsole))
0xC000
(+ 0xC000 0)
(filter #(not (zero? (second %)))(map-indexed vector (:memory NESConsole)))



