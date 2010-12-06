; Copyright (c) Michael Wu.  All rights reserved.
; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(ns org.jfugue.OscPlayer
  (:import (org.jfugue MovableDoNotation Pattern Note MusicStringParser IntervalNotation MusicStringParser MidiRenderer)
           (javax.sound.midi Sequencer Sequence Transmitter Receiver ShortMessage MidiSystem)
           (java.net InetAddress)
           (com.illposed.osc OSCPort OSCPortOut OSCPortIn OSCBundle OSCMessage OSCListener))
  (:use clojure.contrib.command-line)
  (:gen-class
   :init init
   :constructors {[] []}
   :state state
   :methods [[play [String] void]
             [play [org.jfugue.Pattern] void]
             [getSequence [org.jfugue.Pattern] javax.sound.midi.Sequence]]))

(defn -init []
  (let [m {:parser (MusicStringParser.) :renderer (MidiRenderer. Sequence/PPQ 120)}]
    (.addParserListener (:parser m) (:renderer m))
    [[] (atom m)]))

(defn -getSequence [this pattern]
  (let [{:keys [parser renderer]} @(.state this)]
    (.reset renderer)
    (.parse parser pattern)
    (.getSequence renderer)))

(defn -play-String [this music-string]
  (.play this (Pattern. music-string)))

(defn output-short-message [m type]
  (println type ": " (.getData1 m) " " (.getData2 m) " chan: " (.getChannel m)))

(defn decode-short-message [m osc-out]
  (condp = (.getCommand m)
      (ShortMessage/NOTE_ON) (do (.send osc-out (OSCMessage. "/mw10013/m4l/track/1/note" (object-array [(.getData1 m) (.getData2 m)])))
                                 (output-short-message m "noteon"))
      (ShortMessage/NOTE_OFF) (do (.send osc-out (OSCMessage. "/mw10013/m4l/track/1/note" (object-array [(.getData1 m) 0])))
                                  (output-short-message m "noteoff"))
      (ShortMessage/CONTROL_CHANGE) (output-short-message m "cc")
      (output-short-message m "unknown")))

(defn -play-Pattern [this pattern]
  (with-open [osc-out (OSCPortOut. (java.net.InetAddress/getLocalHost) 5432)]
    (let [sequence (.getSequence this pattern)
          sequencer (MidiSystem/getSequencer false)]
      (.open sequencer)
      (.. sequencer (getTransmitter)
          (setReceiver (reify Receiver
                              (send [this midi-message timestamp]
                                    (when (instance? ShortMessage midi-message) (decode-short-message midi-message osc-out))))))
      (doto sequencer (.setSequence sequence) (.start))
      (while (.isRunning sequencer) (Thread/sleep 20))
      (.close sequencer))))

; Packets seems to get dropped if not in bundle.
(defn osc-m4l [bundle?]
  (with-open [osc-out (OSCPortOut. (java.net.InetAddress/getLocalHost) 5432)]
    (let [messages [(OSCMessage. "/mw10013/m4l/track/1/path" (object-array ["path" "live_set" "tracks" 0 "clip_slots" 0 "clip"]))
                    (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "select_all_notes"]))
                    (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "replace_selected_notes"]))
                    (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "notes" 1]))
                    (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "note" 60 (Float. 1.0)  (Float. 0.25) 100 0]))
                    (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "done"]))]]
      (if bundle?
        (let [bundle (OSCBundle.)]
          (doseq [m messages] (.addPacket bundle m))
          (.send osc-out bundle))
        (doseq [m messages] (.send osc-out m))))))

(alter-var-root #'*out* (constantly *out*)) 

(defn -main [& args]
  (with-command-line args
      "OscPlay: use scale degrees: 1 3 5 instead of <1> <5> <8>"
      [[note "The root note for the pattern" 60] strings]
      (let [note (Note. (Integer. note))
            osc-player (org.jfugue.OscPlayer.)]
        (doseq [s strings]
          (let [music-string (.. (org.jfugue.MovableDoNotation. s) (getPatternForRootNote note) (getMusicString))]
            (println s ": " music-string)
            (.play osc-player music-string))))))

