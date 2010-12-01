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
           (com.illposed.osc OSCPort OSCPortOut OSCPortIn OSCMessage OSCListener))
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
      (ShortMessage/NOTE_ON) (do (.send osc-out (OSCMessage. "/renoise/trigger/note_on" (object-array [-1 -1 (.getData1 m) (.getData2 m)])))
                                 (output-short-message m "noteon"))
      (ShortMessage/NOTE_OFF) (do (.send osc-out (OSCMessage. "/renoise/trigger/note_off" (object-array [-1 -1 (.getData1 m)])))
                                  (output-short-message m "noteoff"))
      (ShortMessage/CONTROL_CHANGE) (output-short-message m "cc")
      (output-short-message m "unknown")))

(defn -play-Pattern [this pattern]
  (with-open [osc-out (OSCPortOut. (java.net.InetAddress/getLocalHost) 8000)]
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

