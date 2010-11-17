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

(ns org.jfugue.test.MovableDoNotation
  (:use [org.jfugue.MovableDoNotation] :reload)
  (:use [clojure.test])
  (:import (org.jfugue Pattern Note MusicStringParser Player IntervalNotation)))

(defn interval-music-string [s n]
  (.. (IntervalNotation. s) (getPatternForRootNote n) (getMusicString)))

(defn movable-do-music-string [s n]
  (.. (org.jfugue.MovableDoNotation. s) (getPatternForRootNote n) (getMusicString)))

(defn compare-music-strings [s1 s2]
   (let [n (Note. 60)]
    (is (= (movable-do-music-string s1 n) (interval-music-string s2 n)))))

(deftest try-1
  (let [n (Note. 60)]
    (is (= (interval-music-string "<1> <5> <8>" n) "[60] [64] [67] "))
    (is (= (movable-do-music-string "1 3 5" n) "[60] [64] [67] "))
    (compare-music-strings "<3 3 >3" "<-7> <5> <17>")
    (compare-music-strings "1 3 5" "<1> <5> <8>")
    (compare-music-strings "1 2 3 4 5 6 7 8 9 10 11 12" "<1> <3> <5> <6> <8> <10> <12> <13> <15> <17> <18> <20>")
    (compare-music-strings "1h" "<1>h")
    (compare-music-strings "1h+2h+3q_4qw" "<1>h+<3>h+<5>q_<6>qw")
    (compare-music-strings "T[Allegro] V0 I0 1q 3q V1 1q 3q" "T[Allegro] V0 I0 <1>q <5>q V1 <1>q <5>q")
    (compare-music-strings "V0 1majw V1 I[Flute] 3q 1q" "V0 <1>majw V1 I[Flute] <5>q <1>q")
    (compare-music-strings "T120 V0 I[Piano] 1q 3q V9 [Hand_Clap]q Rq" "T120 V0 I[Piano] <1>q <5>q V9 [Hand_Clap]q Rq")

    (compare-music-strings "<1" "<-11>")
    (compare-music-strings ">1" "<13>")
    (compare-music-strings "<1 1 >1" "<-11> <1> <13>")
    (compare-music-strings ">1h <1h" "<13>h <-11>h")
    (compare-music-strings ">1h+>2h+>3q_>4qw" "<13>h+<15>h+<17>q_<18>qw")
    (compare-music-strings "<1h+<2h+<3q_<4qw" "<-11>h+<-9>h+<-7>q_<-6>qw")
    (compare-music-strings "V0 >1majw V1 I[Flute] <3q <1q" "V0 <13>majw V1 I[Flute] <-7>q <-11>q")
    (compare-music-strings "T120 V0 I[Piano] <1q >3q V9 [Hand_Clap]q Rq" "T120 V0 I[Piano] <-11>q <17>q V9 [Hand_Clap]q Rq")
    ))
