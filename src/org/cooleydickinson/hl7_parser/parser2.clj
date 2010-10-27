;;
;; Provides functions for parsing HL7 messages.
;;
(ns org.cooleydickinson.hl7-parser.parser2
  (:use
   [clojure.contrib.logging]
   [org.cooleydickinson.hl7-parser.util])
  (:import
   (java.text SimpleDateFormat)
   (java.util Date)
   (java.io PushbackReader StringReader)
   (org.apache.commons.logging Log)
   (org.apache.commons.logging LogFactory)))

(defn TEST-MESSAGE
  "Returns a test message with a unique message id."
  []
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
       (. (new Date) getTime) "|P|2.3
PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA~102 Main Street^Apartment #2^Palo Alto^CA^11234^USA|Fax^413&555&6767~Phone^716&232&8989||||||
PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718
ORC|NW|20061019172719
OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"))

;; HL7 Messaging v2.x segment delimiter
(def *SEGMENT-DELIMITER* 10)

;;
;; Data structures used to build a message
;;

(defstruct
    #^{:doc "Structure for HL7 message delimiters"}
  delimiters-struct :field :component :subcomponent :repeating :escape)

(defstruct
    #^{:doc "Structure for an HL7 message"}
  message-struct :delimiters :segments)

(defstruct
    #^{:doc "Structure for an HL7 segment"}
  segment-struct :id :fields)

(defstruct
    #^{:doc "Structure for an HL7 field. The :content will either be
     at atom, an array of atoms (indicating a field with components),
     an array of arrays of atoms (indicating a field with components
     and sub-components) or an array of more field
     structures (indicating a repeating field)."}
  field-struct :content)

;;
;; Emit methods used to output messages
;;

(defn pr-delimiters
  [delimiters-struct]
  (print (str (char (:component delimiters-struct))
              (char (:repeating delimiters-struct))
              (char (:escape delimiters-struct))
              (char (:subcomponent delimiters-struct)))))

(defn pr-content
  [delimiters content]
  (if (coll? content)
    (apply str
           (interpose (char (:subcomponent delimiters)) content))
    content))

(defn pr-field
  [delimiters field]
  (let [content (:content field)]
    (cond

      (= 0 (count content))
      ""

      (map? (first content))
      (apply str
             (interpose (char (:repeating delimiters))
                        (map (partial pr-field delimiters) content)))

      :else
      (apply str
             (interpose (char (:component delimiters))
                        (map (partial pr-content delimiters) content))))))

(defn pr-segment
  [delimiters segment]
  (if (not= "MSH" (:id segment))

      (str (:id segment) (char (:field delimiters))
       (apply str
              (interpose (char (:field delimiters))
                         (map (partial pr-field delimiters) (:fields segment)))))

      (str (:id segment) (char (:field delimiters))
           (:content (first (:fields segment))) (char (:field delimiters))
       (apply str
              (interpose (char (:field delimiters))
                         (map (partial pr-field delimiters)
                              (rest (:fields segment))))))))

(defn pr-message
  [message]
  (print (apply str
                (interpose (char *SEGMENT-DELIMITER*)
                           (map (partial pr-segment (:delimiters message))
                                (:segments message))))))

;;
;; Construction methods used to build messages
;;

(defn create-message
  []
  (struct-map message-struct :delimiters nil :segments []))

(defn create-segment
  [id]
  (struct-map segment-struct :id id :fields []))

(defn create-field
  [data]
  (struct-map field-struct :content data))

(defn add-segment
  [message segment]
  (assoc message :segments (conj (:segments message) segment)))

(defn add-field
  [segment field]
  (assoc segment :fields (conj (:fields segment) field)))

(defn add-fields
  [segment fields]
  (assoc segment :fields (into (:fields segment) fields)))

;;
;; Parser methods
;;

(defmulti get-reader
  "Returns a PushBackReader for the provided Object. We want to wrap
  another Reader but we'll cast to a String and read that if
  required."
  :class)

(defmethod get-reader Readable
  [reader-in]
  (PushbackReader. reader-in))

(defmethod get-reader :default
  [text-in]
  (PushbackReader. (StringReader. (apply str text-in))))

(defn peek-int
  "Returns the next integer that will be read. You can only peek ahead
  one integer."
  [reader]
  
  (let [next-int (.read reader)]
    (.unread reader next-int)
    next-int))

(defn expect-char-int
  "Returns true if the int-in matches the char-expect-in and false if
  it does not. An exception will be thrown if the int-in has a value
  of -1 or is an invalid character."
  [char-expect-int int-in]

  (if (= -1 int-in)
    (throw (Exception. (str "End of file reached while looking for " (char char-expect-int))))
    (if (= char-expect-int int-in)
      true
      (throw (Exception. (str "Expected \"" (char char-expect-int) "\" but read "
                              "\"" (char int-in) "\""))))))

(defn delimiter?
  "Returns true if the provided Integer corresponds to the character
  value of one of this messages delimiters."
  [message int-in]

  (if (= -1 int-in)
    (throw (Exception. "End of data reached while reading text")))

  (if (or (= (:component (:delimiters message)) int-in)
          (= (:repeating (:delimiters message)) int-in)
          (= (:subcomponent (:delimiters message)) int-in)
          (= (:field (:delimiters message)) int-in)
          (= (:escape (:delimiters message)) int-in)
          (= *SEGMENT-DELIMITER* int-in))
    true false))

(defn read-delimiters
  "Parsers through the MSH segment up until the end of the first
  field (the list of delimiters) and returns a map of the delimiter
  values (delimiter-struct)."
  [reader]

  (loop [int-in (.read reader) buffer [] delimiters (struct-map delimiters-struct) char-index 0]

    (cond

      (= -1 int-in)
      (throw (Exception. "End of file reached while reading MSH segment"))

      (= *SEGMENT-DELIMITER* int-in)
      (throw (Exception. "End of segment reached while reading MSH segment"))

      (= 3 char-index)
      (let [segment-id (apply str buffer)]
        (if (not (= "MSH" segment-id))
          (throw (Exception. (str "Expected first segment to have the id of \"MSH\" but found "
                                  "\"" segment-id "\""))))
        (recur (.read reader) buffer (assoc delimiters :field int-in) (inc char-index)))

      (= 4 char-index)
      (recur (.read reader) buffer (assoc delimiters :component int-in) (inc char-index))

      (= 5 char-index)
      (recur (.read reader) buffer (assoc delimiters :repeating int-in) (inc char-index))

      (= 6 char-index)
      (recur (.read reader) buffer (assoc delimiters :escape int-in) (inc char-index))

      (= 7 char-index)
      (recur (.read reader) buffer (assoc delimiters :subcomponent int-in) (inc char-index))

      (= 8 char-index)
      (do
        (if (not (expect-char-int (:field delimiters) int-in))
          (throw (Exception. "Expected beginning of next segment but read more delimiter data")))
        (.unread reader int-in)
        delimiters)

      :else
      (recur (.read reader) (conj buffer (char int-in)) delimiters (inc char-index)))))

(defn read-text
  "Reads in text up to the next delimiter character."
  [message reader]

  (loop [int-in (.read reader) buffer []]

    (cond

      (= int-in -1)
      (throw (Exception. "End of data reached while reading text"))

      (delimiter? message int-in)
      (do (.unread reader int-in)
          (apply str buffer))

      :else
      (recur (.read reader) (conj buffer (char int-in))))))


(defn read-subcomponents
  "Reads in the field subcomponent data from the reader."
  [reader message data]

  (expect-char-int (:subcomponent (:delimiters message)) (.read reader))

  (loop [int-in (.read reader)
         subcomponents (if (not (nil? data)) [data] [])
         subcomponent []]

    (cond

      (= (:subcomponent (:delimiters message)) int-in)
      (recur (.read reader) (conj subcomponents (apply str subcomponent)) [])

      (or (= *SEGMENT-DELIMITER* int-in)
          (= (:field (:delimiters message)) int-in)
          (= (:component (:delimiters message)) int-in)
          (= (:repeating (:delimiters message)) int-in))
      (do (.unread reader int-in)
          (conj subcomponents (apply str subcomponent)))

      :else
      (recur (.read reader) subcomponents (conj subcomponent (char int-in))))))

(defn read-field
  "Reads in the next field of segment data from the reader."
  [reader message]

  (let [int-in (.read reader)]
    (if (not (or (= (:field (:delimiters message)) int-in)
                 (= (:repeating (:delimiters message)) int-in)))
      (throw (Exception. "Expected a field or repeating delimiter when reading field data"))))

  (loop [int-in (.read reader) field-data [] current-field nil]

    (cond

      ;; handle repeating fields
      (= (:repeating (:delimiters message)) int-in)
      (do (.unread reader int-in)
          (recur nil
                 (let [repeating-data
                       (if (not (map? (first field-data)))
                         [(create-field
                           (if (not (nil? current-field))
                             (conj field-data (apply str current-field))
                             field-data))]
                         field-data)]
                   (conj repeating-data (read-field reader message)))
                 []))
      
      ;; handle subcomponents
      (= (:subcomponent (:delimiters message)) int-in)
      (do (.unread reader int-in)
          (recur nil
                 (conj field-data (read-subcomponents
                                   reader message
                                   (if (not (nil? current-field))
                                     (apply str current-field)
                                     nil)))
                 nil))

      ;; handle components
      (= (:component (:delimiters message)) int-in)
      (recur (.read reader)
             (if (not (nil? current-field))
               (conj field-data (apply str current-field)) field-data)
             [""])

      ;; handle the end of the field
      (or (= *SEGMENT-DELIMITER* int-in)
          (= (:field (:delimiters message)) int-in)
          (= -1 int-in))
      (do

        ;; don't unread the end of file marker
        (if (not= -1 int-in)
          (.unread reader int-in))

        ;; create our field
        (create-field

         ;; if we have current field data, add that to our field data
         (if (< 0 (count current-field))
           (if (not (nil? current-field))
             (conj field-data (apply str current-field)) field-data)
           field-data)))

      :else
      (recur (.read reader) field-data
             (if int-in
               (if (not (nil? current-field))
                 (conj current-field (char int-in)) [(char int-in)])
               current-field)))))

(defn read-msh-segment
  "Adds the \"MSH\" segment and its first field of data to the
  provided message-struct and returns the new message. This first
  field will be the list of delimiters, the provided message must
  already have a valid set of delimiters."
  [reader message]

  (let [segment (add-field (create-segment "MSH")
                           (create-field (with-out-str (pr-delimiters (:delimiters message)))))]

    (loop [int-in (.read reader) fields []]

      (cond

        (= -1 int-in)
        (throw (Exception. "End of file reached while reading segment data"))

        (= (:field (:delimiters message)) int-in)
        (do (.unread reader int-in)
            (recur nil (conj fields (read-field reader message))))

        (= *SEGMENT-DELIMITER* int-in)
        (do (.unread reader int-in)
            (add-segment message (add-fields segment fields)))

        :else
        (recur (.read reader) fields)))))

(defn read-segment
  [reader message]

  (expect-char-int *SEGMENT-DELIMITER* (.read reader))

  (let [segment-id (read-text message reader)]

    (if (or (nil? segment-id) (not= 3 (count segment-id)))
      (throw (Exception. (str "Illegal segment id \"" segment-id "\" read"))))

    (let [segment (create-segment segment-id)]

      (loop [int-in (.read reader) fields []]

        (cond

          (= -1 int-in)
          (add-segment message (add-fields segment fields))

          (= *SEGMENT-DELIMITER* int-in)
          (do (.unread reader int-in)
              (add-segment message (add-fields segment fields)))

          (= (:field (:delimiters message)) int-in)
          (do (.unread reader int-in)
              (recur nil (conj fields (read-field reader message))))

          :else
          (recur (.read reader) fields))))))

(defn parse-message
  "Parsers the data read by the reader into a valid HL7 message data
  structure (message-struct)."
  [reader]

  (loop [int-in (.read reader) parsing :delimiters message (create-message)]

    (cond

      (or (= -1 int-in)
          (and (nil? int-in) (= -1 (peek-int reader))))
      (recur nil :complete message)

      (= parsing :delimiters)
      (do (.unread reader int-in)
          (recur nil :msh-segment
                 (assoc message :delimiters (read-delimiters reader))))

      (= parsing :msh-segment)
      (recur nil :segment (read-msh-segment reader message))

      (= parsing :segment)
      (recur nil :segment (read-segment reader message))

      :complete message

      :else
      (recur (.read reader) parsing message))))

(defn parse
  "Reads data from the provided source (a Reader, String, etc.) and
  parses that data into a hash-map (hl7-struct) that represents the
  content of the message."
  [message-source]
  (parse-message (get-reader message-source)))