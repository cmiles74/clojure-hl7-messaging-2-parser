(defproject org.clojars.cmiles74/clojure-hl7-parser "3.4.5"
  :description "A parser for parsing HL7 messages."
  :url "https://github.com/cmiles74/clojure-hl7-messaging-2-parser"
  :main com.nervestaple.hl7-parser.main
  :repositories [["clojars" {:url "https://repo.clojars.org" :cred :gpg}]]
  :profiles {:dev {:dependencies [[lambdaisland/kaocha "1.77.1236"]]}}
  :aliases {"kaocha" ["run" "-m" "kaocha.runner"]})
