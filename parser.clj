;;
;; Provides functions for parsing HL7 messages.
;;

(ns org.cooleydickinson.messagehub.parser
  (:use
   [clojure.contrib.logging])
  (:import
   (java.text SimpleDateFormat)
   (java.util Date)
   (org.apache.commons.logging Log)
   (org.apache.commons.logging LogFactory)))

;; our ASCII codes
(def ASCII_VT 11)
(def ASCII_FS 28)
(def ASCII_CR 13)
(def ASCII_LF 10)

;; message segment field names
(def FIELD-NAMES
     {:MSH {9 "id"}})

;; timestamp format
(def TIMESTAMP-FORMAT (new SimpleDateFormat "yyyMMddHHmmss"))

(defn TEST-MESSAGE
  "Returns a test message with a unique message id."
  []
  (str "MSH|^~0\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3
PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||
PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718
ORC|NW|20061019172719
OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"))

(defn sanitize-message
  "Removes all control characters from a message."
  [message]
  (. message replaceAll "\\p{Cntrl}" ""))

(defn int-to-segment-field-name
  "Returns the name of the field that corresponds to the given field
  number on the message of the supplied type."
  [segment-type segment-field-number]
  segment-field-number)

(defn parse-fields
  "Parses the fields of segment into a list of values."
  [field]
  (seq (. field (split "\\^"))))

(defn nth-or-nil
  "Returns the nth item in the sequence or nil if the sequence doesn't
  have an nth item. This differs from the standard nth function
  because it won't through an index out of bounds exception."
  [sequence index]
  (if (> (count sequence) index)
    (nth sequence index)
    nil))

(defn pretty-name-for-segment
  "Parses a segment of (parsed) HL7 name data into a pretty String
  containing the name. This will be in the format...

    Last, First Middle Suffix Prefix Degree

  When I say the HL7 name data should be parsed, I mean that the
  entire messages should be the result of a 'parse-message' call."
  [fields]

  (str (. (nth-or-nil fields 0) trim)
       ", "
       (. (str (nth-or-nil fields 1) " "
               (. (str (nth-or-nil fields 2) " "
                       (. (str (nth-or-nil fields 3) " ") trim)
                       (nth-or-nil fields 4) " ") trim)) trim)))

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

        (if (. field contains "^")

          (swap! parsed-segment assoc
                 (int-to-segment-field-name @segment-type
                                            @field-counter)
                 (parse-fields field))
          
          (swap! parsed-segment assoc
                 (int-to-segment-field-name @segment-type
                                            @field-counter)
                 field))

        (swap! field-counter inc))
      
      (if (< 0 (count (rest fields)))
        (recur (rest fields))))

    @parsed-segment))

(defn parse-message
  "Parses an HL7 message into a list of segments. Each segment will be
  a hash-map of fields and values."
  [message]

  (let [parsed-message (atom (list))]

    (loop [segments (. message split "\n|\r")]

      (reset! parsed-message (conj @parsed-message
                                   (parse-segment (first segments))))
      
      (if (< 0 (count (rest segments)))
        (recur (rest segments))))

    (reverse @parsed-message)))

(defn ack-message
  "Returns an acknowledgement message for the given message. If the
  message indicates that no acknowledgement should be returned, this
  function returns nil."
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

(defn msh-segment
  "Returns the MSH segment of the message. The segment will be
  returned as a hash-map, they keys will be the number of the
  segment."
  [message]

  (let [parsed-message (parse-message message)]
    (first parsed-message)))

(defn message-id
  "Returns the message id for the provided message."
  [message]

  ;; parse the message
  (let [msh-segment (msh-segment message)]

    ;; return the message id
    (msh-segment 9)))
