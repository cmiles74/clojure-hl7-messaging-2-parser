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
  (let [message (parser/TEST-MESSAGE)
        parsed-message (parser/parse-message message)]

    ;; provide a brief demonstration
    (info (str "Message Id (unparsed message): "
               (parser/message-id-unparsed message)))
    (info (str "Message Id: " (parser/message-id parsed-message)))
    (info (str "MSH Segment: " (parser/msh-segment parsed-message)))
    (info (str "ACK: " (parser/ack-message parsed-message)))
    (info (str "Parsed: " parsed-message))))

(defn -main
  "Provides the main function needed to bootstrap the application."
  [& args]
  (main args))

