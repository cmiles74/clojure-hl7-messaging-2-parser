;;
;; Main entry-point for the application.
;;

(ns org.cooleydickinson.hl7-parser.main
  (:gen-class)
  (:use
   [clojure.contrib.logging]
   [org.cooleydickinson.hl7-parser.parser :as parser])
  (:import
   (org.apache.commons.logging Log)
   (org.apache.commons.logging LogFactory)))

;; logger instance
(def *logger* (. LogFactory getLog "org.cooleydickinson.messagehub.main"))

(defn main
  "Provides the main function invoked when the application starts. We
  use this method so that we can test startup."
  [& args]

  ;; get a test message
  (let [message (parser/TEST-MESSAGE)]

    ;; provide a brief demonstration
    (info (str "Message Id: " (parser/message-id message)))
    (info (str "MSH Segment: " (parser/msh-segment message)))
    (info (str "ACK: " (parser/ack-message message)))
    (info (str "Parsed: " (parser/parse-message message)))))

(defn -main
  "Provides the main function needed to bootstrap the application."
  [& args]
  (main args))

