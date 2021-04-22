;;
;; Provides simple test functions meant to demonstrate how the parser
;; works.
;;

(ns com.nervestaple.hl7-parser.main
  (:gen-class)
  (:use
   [taoensso.timbre :as timbre
    :only (trace debug info warn error fatal spy)]
   [clojure.java.io :as io]
   [com.nervestaple.hl7-parser.parser :as parser]
   [com.nervestaple.hl7-parser.message :as message]
   [com.nervestaple.hl7-parser.batch :as batch]
   [com.nervestaple.hl7-parser.test :as test]))

(defn main
  "Provides the main function invoked when the application starts. We
  use this method so that we can test startup."
  [& args]

  ;; get a test message
  (let [message (test/test-message)
        parsed-message (parser/parse message)]

    ;; provide a brief demonstration
    (info (str "Message Id: "
               (message/get-field-first-value parsed-message "MSH" 10)))
    (info (str "MSH Segment: "
               (pr-str (first (get-segments parsed-message "MSH")))))
    (info (str "ACK: "
               (message/ack-message {:sending-app "Clojure HL7 Parser"
                                     :sending-facility "Test Facility"
                                     :production-mode "P"
                                     :version "2.3"
                                     :text-message "Message processed successfully"}
                                    "AA" parsed-message))
    (info (str "Parsed: " (pr-str parsed-message))))))

(defn -main
  "Provides the main function needed to bootstrap the application."
  [& args]
  (main args))
