(defproject clojure-hl7-parser "1.0-SNAPSHOT"
  :description "A parser for parsing HL7 messages."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [commons-logging/commons-logging "1.1.1"]]
  :dev-dependencies [[swank-clojure "1.1.0"]]
  :repositories [["cdh" "http://cdhintranet01.cooley-dickinson.org/nexus/content/groups/public"]]
  :main "org.cooleydickinson.hl7_parser.main")