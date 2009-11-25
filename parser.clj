;;
;; Provides functions for parsing HL7 messages.
;;

(ns org.cooleydickinson.messagehub.parser
  (:use
   [clojure.contrib.logging]))

;; our ASCII codes
(def ASCII_VT 11)
(def ASCII_FS 28)
(def ASCII_CR 13)
(def ASCII_LF 10)

(def TEST-MESSAGE "MSH|^~\\&|System|HIS|HL7Genie|Hosp|20050804162010||ADT^A01|Message Control ID|P|2.3.1|||AL|AL
EVN|A01|199901061000|199901101400|01||199901061000
PID|||500515|123121|TEST^PCP^^^^||19490125|F||W|PO BOX 89^^GILBERTVILLE^MA^01031^^|WOR
PV1||O|LB|3|||000298^SILVERSTEIN^SUZY^S^^MD|||LB||||1|||000298^SILVERSTEIN^SUZY^S^^MD|
GT1|1||TEST^PCP^||PO BOX 89^^GILBERTVILLE^MA^01031^^|||19490125|F||P|559-62-0314|||1||
IN1|1||428|HEALTH NEW ENGLAND|ONE MONARCH PLACE^^SPRINGFIELD^MA^01144^^|||||||||||TEST
IN2||559-62-0314|^|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||^
IN1|2||200|SELF PAY AFTER INSURANCE|^^^^^^|||||||||||TEST^PCP^|18|19490125|^^^^^^|||||
IN2||559-62-0314|^|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||^^||18|")

(defn int-to-hl7-segment-field-name
  "Returns the name of the field that corresponds to the given field
  number on the message of the supplied type."
  [segment-type
  segment-field-number] segment-field-number)

(defn parse-hl7-segment
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
               (int-to-hl7-segment-field-name @segment-type
                                              @field-counter)
               field)

        (swap! field-counter inc))
      
      (if (< 0 (count (rest fields)))
        (recur (rest fields))))

    @parsed-segment))

(defn parse-hl7-message
  "Parses an HL7 message into a list of segments. Each segment will be
  a hash-map of fields and values."
  [message]

  (let [parsed-message (atom (list))]

    (loop [segments (. message split "\n")]

      (reset! parsed-message (conj @parsed-message
                                   (parse-hl7-segment (first segments))))
      
      (if (< 0 (count (rest segments)))
        (recur (rest segments))))

    (reverse @parsed-message)))

(defn ack-hl7-message
  [message]

  ;; parse the message
  (let [parsed-message (parse-hl7-message message)]

    ;; verify that the sender is looking for an ack
    (if (or (not (= "NE" ((first parsed-message) 15)))
            (not (= "ER" ((first parsed-message) 15))))

      ;; return our ack
      (str (char ASCII_VT)
           "MSA|AA|"
           ((first parsed-message) 9) "|"
           "Message Recieved Successfully|"
           (char ASCII_FS) (char ASCII_CR))
      
      ;; return null
      nil)))
