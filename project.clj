(defproject jfugue-movabledo "0.0.5"
  :description "Movable do support for JFugue (http://www.jfugue.org)"
  :license {:name "GNU Lesser General Public License 2.1"}
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [jfugue "4.0.3"]
                 [org.clojars.mw10013/javaosc "20060402.0.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :aot [org.jfugue.MovableDoNotation org.jfugue.OscPlayer]
  :main org.jfugue.MovableDoNotation)

