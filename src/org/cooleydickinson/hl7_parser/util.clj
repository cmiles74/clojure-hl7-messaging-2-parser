;;
;; Utility functions related to HL7 messages that don't really belong
;; anywhere else.
;;
(ns org.cooleydickinson.hl7-parser.util
  (:import
   (java.util Date)))

(defn TEST-MESSAGE
  "Returns a test message with a unique message id."
  []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3
PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA~102 Main Street^Apartment #2^Palo Alto^CA^11234^USA|Fax^413&555&6767~Phone^716&232&8989||||||
PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718
ORC|NW|20061019172719
OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"))