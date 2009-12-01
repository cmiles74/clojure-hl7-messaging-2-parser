;;
;; Provides functions for parsing HL7 messages.
;;

(ns org.cooleydickinson.messagehub.parser
  (:use
   [clojure.contrib.logging])
  (:import
   (java.text SimpleDateFormat)
   (java.util Date)))

;; our ASCII codes
(def ASCII_VT 11)
(def ASCII_FS 28)
(def ASCII_CR 13)
(def ASCII_LF 10)

;; timestamp format
(def TIMESTAMP-FORMAT (new SimpleDateFormat "yyyMMddHHmmss"))

(def TEST-MESSAGE "MSH|^~0\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|MSGID12349876|P|2.3
PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||
PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718
ORC|NW|20061019172719
OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876")

(defn sanitize-message
  "Removes all control characters from a message."
  [message]
  (. message replaceAll "\\p{Cntrl}" ""))

(defn int-to-segment-field-name
  "Returns the name of the field that corresponds to the given field
  number on the message of the supplied type."
  [segment-type
  segment-field-number] segment-field-number)

(defn parse-segment
  "Parses an HL7 message segment into a hash-map of values. The name
  of the field will be the key, unless the name is unknown in which
  case the key will be the field index number."
  [segment]

  (let [field-counter (atom 0)
        parsed-segment (atom (sorted-map))
        segment-type (atom nil)]

    (loop [fields (. segment split "\\|")]

      (let [field (first fields)]

        (if (= 0 @field-counter)
          (reset! segment-type field))

        (swap! parsed-segment assoc
               (int-to-segment-field-name @segment-type
                                              @field-counter)
               field)

        (swap! field-counter inc))
      
      (if (< 0 (count (rest fields)))
        (recur (rest fields))))

    @parsed-segment))

(defn parse-message
  "Parses an HL7 message into a list of segments. Each segment will be
  a hash-map of fields and values."
  [message]

  (let [parsed-message (atom (list))]

    (loop [segments (. message split "\n")]

      (reset! parsed-message (conj @parsed-message
                                   (parse-segment (first segments))))
      
      (if (< 0 (count (rest segments)))
        (recur (rest segments))))

    (reverse @parsed-message)))

(defn ack-message
  [message]

  ;; parse the message
  (let [parsed-message (parse-message message)
        msh-segment (first parsed-message)]

    ;; verify that the sender is looking for an ack
    (if (or (not (= "NE" ((first parsed-message) 15)))
            (not (= "ER" ((first parsed-message) 15))))

      ;; return our ack
      (str (char ASCII_VT)
           "MSH|" (msh-segment 1) "|MSGHUB|" (msh-segment 3) "|"
           (msh-segment 2) "|" (msh-segment 5) "|"
           (. TIMESTAMP-FORMAT format (new Date)) "||ACK^001|"
           (msh-segment 9) "|P|2.3"
           (char ASCII_CR)
           "MSA|AA|"
           (msh-segment 9) "|"
           "Message Recieved Successfully|"
           (char ASCII_FS) (char ASCII_CR))
      
      ;; return null
      nil)))


