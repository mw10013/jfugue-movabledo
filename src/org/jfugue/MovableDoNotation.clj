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

(ns org.jfugue.MovableDoNotation
  (:import (org.jfugue Pattern Note MusicStringParser Player IntervalNotation MusicStringParser MidiRenderer)
           (javax.sound.midi Sequencer Sequence Transmitter Receiver ShortMessage MidiSystem))
  (:use clojure.contrib.command-line)
  (:gen-class
   :init init
   :constructors {[String] []}
   :state state
   :methods [[getMusicStringWithMovableDo [] String]
             [setMusicStringWithMovableDo [String] void]
             [getPatternForRootNote [String] org.jfugue.Pattern]
             [getPatternForRootNote [org.jfugue.Pattern] org.jfugue.Pattern]
             [getPatternForRootNote [org.jfugue.Note] org.jfugue.Pattern]]))

(def ^{:private true} degree-vector [0 2 4 5 7 9 11])

(defn -init [music-string]
  [[] (atom {:music-string music-string})])

(defn -getMusicStringWithMovableDo [this]
  (:music-string @(.state this)))

(defn -setMusicStringWithMovableDo [this music-string]
  (swap! (.state this) assoc :music-string music-string))

(defn -getPatternForRootNote-String [this music-string]
  (.getPatternForRootNote this (Pattern. music-string)))

(defn -getPatternForRootNote-Pattern [this pattern]
  (.getPatternForRootNote this (MusicStringParser/getNote pattern)))

(defn format-degree [{:keys [degree root-note-value offset]}]
  (let [degree-normalized (dec degree) degree-vector-count (count degree-vector)]
    (str "[" (+  root-note-value offset
                 (* 12 (quot degree-normalized degree-vector-count)) 
                 (nth degree-vector (mod degree-normalized degree-vector-count)))  "]")))
 
(defn parse-char [m c]
;  (println "m:" m)
;  (println "c:" c ":")
  (condp = (:state m)
      :degree (cond
               (Character/isDigit c) (assoc m :degree (+ (* (:degree m) 10) (Character/digit c 10)))
               (= c \<) (assoc m :offset (+ (:offset m) -12))
               (= c \>) (assoc m :offset (+ (:offset m) 12))
               (Character/isWhitespace c) (assoc m :state :whitespace
                                                 :buf (conj (:buf m) (format-degree m) " ") :degree 0 :offset 0)
               :else (assoc m :state :char
                            :buf (conj (:buf m) (if (= (:degree m) 0) "" (format-degree m)) c) :degree 0 :offset 0))
      :whitespace (cond
                   (Character/isDigit c) (assoc m :state :degree :degree (+ (* (:degree m) 10) (Character/digit c 10)))
                   (= c \<) (assoc m :state :degree :offset (+ (:offset m) -12))
                   (= c \>) (assoc m :state :degree :offset (+ (:offset m) 12))
                   (Character/isWhitespace c) m
                   :else (assoc m :state :char :buf (conj (:buf m) c)))
      :char (cond
             (or (= c \+) (= c \_)) (assoc m :state :degree :buf (conj (:buf m) c))
             (Character/isWhitespace c) (assoc m :state :whitespace :buf (conj (:buf m) c))
             :else (assoc m :buf (conj (:buf m) c)))))
 
(defn -getPatternForRootNote-Note [this note]
  (let [m (reduce parse-char {:state :degree :root-note-value (.getValue note) :degree 0 :offset 0 :buf []}
                  (str (:music-string @(.state this)) " "))]
    (Pattern. (apply str (:buf m)))))

(alter-var-root #'*out* (constantly *out*)) 

(defn -main [& args]
  (with-command-line args
      "MovableDoNotation: use scale degrees: 1 3 5 instead of <1> <5> <8>"
      [[note "The root note for the pattern" 60]
       [play? p? "Play."] strings]
      (let [note (Note. (Integer. note))
            player (if  play? (Player.) nil)]
        (doseq [s strings]
          (let [music-string (.. (org.jfugue.MovableDoNotation. s) (getPatternForRootNote note) (getMusicString))]
            (println s ": " music-string)
            (if play? (.play player music-string)))))))
