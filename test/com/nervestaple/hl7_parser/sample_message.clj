(ns com.nervestaple.hl7-parser.sample-message
  (:require
   [com.nervestaple.hl7-parser.parser :as parser])
  (:import
   (java.util Date)))

(defn message
  "Returns a short test message."
  []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1676735383748|P|2.3" (char parser/ASCII_CR)
       "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||" (char parser/ASCII_CR)
       "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718" (char parser/ASCII_CR)
       "ORC|NW|20061019172719" (char parser/ASCII_CR)
       "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876" (char parser/ASCII_CR)))

(defn message-no-ack
  "Returns a short test message that doesn't require an acknowledgement."
  []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1676735383748|P|2.3|||NE" (char parser/ASCII_CR)
       "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||" (char parser/ASCII_CR)
       "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718" (char parser/ASCII_CR)
       "ORC|NW|20061019172719" (char parser/ASCII_CR)
       "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876" (char parser/ASCII_CR)))

(defn message-unique-id
  "Returns a short test message with a unique message id."
  []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3" (char parser/ASCII_CR)
       "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||" (char parser/ASCII_CR)
       "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718" (char parser/ASCII_CR)
       "ORC|NW|20061019172719" (char parser/ASCII_CR)
       "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876" (char parser/ASCII_CR)))
