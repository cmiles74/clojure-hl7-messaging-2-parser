;;
;; Provides functions for parsing HL7 messages.
;;

(ns org.cooleydickinson.hl7-parser.parser
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

(def REGEX-MESSAGE-ID
     #"MSH\|[^\|]*\|[^\|]*\|[^\|]*\|[^\|]*\|[^\|]*\|[^\|]*\|[^\|]*\|[^\|]*\|([^\|]*)\|")

(defn message-id-unparsed
  "Returns the message id for an HL7 message by doing some simple
  regular expression matching on the message. This function does *not*
  involve parsing the message and may be faster."
  [message]
  (let [matches (re-find REGEX-MESSAGE-ID message)]
    (if (and matches (second matches))
      (second matches))))

(defn sanitize-message
  "Removes all control characters from a message."
  [message]
  (if message
    (. message replaceAll "\\p{Cntrl}" "")))

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

(defn first-field-value
  "Returns only the first field value if there are multiple fields. If
  there's only one field then that field is returned."
  [fields]

  (if (sequential? fields)
    (first fields)
    fields))

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

(defn extract-text-from-segment
  "Extracts the text from the parsed message for the supplied index of
  the given segment. For instance, this function could extract all of
  the text from the fifth index of the OBX segments:

      (extract-text-from-segment parsed-message 'OBX' 5)"
  [parsed-message segment-type index]

  ;; combine all of the 5th items of the OBX segments into a string
  (apply str

         ;; pick out the OBX segments
         (for [segment parsed-message :when (= (segment 0) segment-type)]

           ;; return the 5th index and append a return
           (str (segment index) "\n"))))

(defn segments
  "Returns the requested segments of the parsed message. For instance,
  if the supplied segment type is 'OBX', all of the OBX segments will
  be returned."
  [parsed-message segment-type]

  (for [segment parsed-message :when (= (segment 0) segment-type)]
    segment))

(defn segment-index-value
  "Returns the value of the provided index in the named segment. If
  the segment appears more than once, the first value is returned."
  [parsed-message segment-type index]

  ;; let value be the first matching segment with the requested index
  (let [value (first
               (for [segment parsed-message :when (= (segment 0) segment-type)]
                 (segment index)))]

    ;; make sure that this is not an empty string or a sequence with
    ;; no length
    (if (and value
             (or (and (string? value)
                      (< 0 (. (. value trim) length)))
                 (and (seq? value)
                      (< 0 (count value)))))

      ;; return our valid value
      value

      ;; return nil to indicate a blank string or empty segment
      nil)))

(defn msh-segment
  "Returns the MSH segment of the message."
  [parsed-message]

  ;; we can return the first MSH because only one is allowed
  (first (segments parsed-message "MSH")))

(defn ack-message
  "Returns an acknowledgement message for the given parsed message. If
  the message indicates that no acknowledgement should be returned,
  this function returns nil."
  [parsed-message]

  ;; parse the message
  (let [msh-segment-parsed (msh-segment parsed-message)]

    ;; verify that the sender is looking for an ack
    (if (or (not (= "NE" (msh-segment-parsed 15)))
            (not (= "ER" (msh-segment-parsed 15))))

      ;; return our ack
      (str (char ASCII_VT)
           "MSH|^~\\&|MSGHUB|" (msh-segment-parsed 3) "|"
           (msh-segment-parsed 2) "|" (msh-segment-parsed 5) "|"
           (. TIMESTAMP-FORMAT format (new Date)) "||ACK|"
           (msh-segment-parsed 9) "|P|2.3|"
           (char ASCII_CR)
           "MSA|AA|"
           (msh-segment-parsed 9) "|"
           "Message Recieved Successfully|" (char ASCII_CR)
           (char ASCII_FS) (char ASCII_CR))

      ;; return null
      nil)))

(defn message-id
  "Returns the message id for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "MSH" 9)))

(defn patient-account-number
  "Returns the patient account number for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "PID" 18)))

(defn medical-record-number
  "Returns the medical record number for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "PID" 3)))

(defn attending-physician
  "Returns the attending physician fields for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "PV1" 7)))

(defn referring-physician
  "Returns the referring physician fields for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "PV1" 8)))

(defn consulting-physician
  "Returns the consulting physician fields for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "PV1" 9)))

(defn admitting-physician
  "Returns the admitting physician fields for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "PV1" 17)))

(defn ordering-provider
  "Returns the ordering-provider fields for the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "ORC" 12)))

(defn observing-ordering-provider
  "Returns the ordering-provider fields from the OBR (observing)
  segment of the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "OBR" 16)))

(defn copy-to-provider
  "Returns the copy-to provider fields from the OBR (observing)
  segment of the provided message."
  [parsed-message]
  (first-field-value
   (segment-index-value parsed-message "OBR" 28)))