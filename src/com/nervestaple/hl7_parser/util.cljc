;;
;; Utility functions related to HL7 messages that don't really belong
;; anywhere else.
;;
(ns com.nervestaple.hl7-parser.util
  #?(:clj
     (:import
      (java.util Date))
     :cljs
     (:require
      [clojure.string :as string])))

(defn sanitize-message
  "Removes all control characters from a message."
  [message]
  (if message
    #?(:clj (. message replaceAll "\\p{Cntrl}" "")
       ;; the same characters as Java's \p{Cntrl}
       :cljs (string/replace message #"[\x00-\x1F\x7F]" ""))))

