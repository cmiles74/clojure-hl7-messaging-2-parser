;;
;; Functions to support testing of the HL7 version 2.x parser.
;;

(ns com.nervestaple.hl7-parser.test
  (:gen-class)
  (:use
   [com.nervestaple.hl7-parser.parser])
  (:import
   (java.util Date)))

(defn test-message
  "Returns a test message with a unique message id."
  []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3" (char ASCII_CR)
       "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||" (char ASCII_CR)
       "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718" (char ASCII_CR)
       "ORC|NW|20061019172719" (char ASCII_CR)
       "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876" (char ASCII_CR)))
