;;
;; Utility functions related to HL7 messages that don't really belong
;; anywhere else.
;;
(ns com.nervestaple.hl7-parser.util
  (:import
   (java.util Date)))

(defn sanitize-message
  "Removes all control characters from a message."
  [message]
  (if message
    (. message replaceAll "\\p{Cntrl}" "")))

