;;
;; Provides functions for parsing HL7 messages.
;;
(ns com.nervestaple.hl7-parser.parser
  (:use
    [clojure.string :as string :only (trim)])
  (:import
   (java.text SimpleDateFormat)
   (java.util Date)
   (java.io PushbackReader StringReader)))

;; HL7 timestamp format
(def TIMESTAMP-FORMAT (new SimpleDateFormat "yyyyMMddHHmmss"))

;; ASCII codes of characters used to delimit and wrap messages
(def ASCII_VT 11)
(def ASCII_FS 28)
(def ASCII_CR 13)
(def ASCII_LF 10)

;; HL7 Messaging v2.x segment delimiter
(def SEGMENT-DELIMITER ASCII_CR)

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
  "Prints an HL7 compatible text representation of the provided
  delimiters to the current *out* stream."
  [delimiters-struct]
  (str (char (:component delimiters-struct))
       (char (:repeating delimiters-struct))
       (char (:escape delimiters-struct))
       (char (:subcomponent delimiters-struct))))

(defn- do-pr-content
  "Returns an HL7 compatible String representation of the provided
  content atom. Only Date objects are afforded special handling, an
  HL7 compatible timestamp is returned."
  [content]
  (if (instance? java.util.Date content)
    (.Format TIMESTAMP-FORMAT content)
    content))

(defn- pr-content
  "Returns an HL7 compatible String representation of the provided
  field content."
  [delimiters content]
  (if (coll? content)
    (apply str
           (interpose (char (:subcomponent delimiters))
                      (map do-pr-content content)))
    (do-pr-content content)))

(defn pr-field
  "Returns an HL7 compatible String representation of the provided
  field."
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
  "Returns an HL7 compatible String representation of the provided
  segment."
  [delimiters segment]
  (if (or (not= "FHS" (:id segment))
          (not= "BHS" (:id segment))
          (not= "MSH" (:id segment)))

      (str (:id segment) (char (:field delimiters))
       (apply str
              (interpose (char (:field delimiters))
                         (map (partial pr-field delimiters) (:fields segment)))))

      (str (:id segment) (char (:field delimiters))
           (first (:content (first (:fields segment)))) (char (:field delimiters))
       (apply str
              (interpose (char (:field delimiters))
                         (map (partial pr-field delimiters)
                              (rest (:fields segment))))))))

(defn str-message
  "Returns the provided HL7 message as a string."
  [message]
  (str (apply str
              (interpose (char SEGMENT-DELIMITER)
                         (map (partial pr-segment (:delimiters message))
                              (:segments message))))
       (char SEGMENT-DELIMITER)))

(defn pr-message
  "Prints the provided HL7 message to the current *out* stream."
  [message]
  (print (str-message message)))

;;
;; Construction methods used to build messages
;;

(defn convert-values
  "If provided a list of values, any in items are replaced with an empty string.
  When provided a single value a list with that value will be returned or an
  empty list of that value is nil."
  [values]
  (cond
    (sequential? values)
    (replace {nil ""} values)

    (nil? values)
    []

    :else
    [values]))

(defn create-empty-message
  "Returns a new, empty message structure."
  []
  (struct-map message-struct :delimiters nil :segments []))

(defn create-message
  "Returns a new, empty message structure."
  [delimiters & segments]
  (struct-map message-struct :delimiters delimiters
              :segments (if (< 0 (count segments)) (vec segments) [])))

(defn create-segment
  "Returns a new, empty segment structure with the provided id."
  [id & fields]
  (struct-map segment-struct :id id
              :fields (if (< 0 (count fields)) (vec fields) [])))

(defn create-field
  "Returns a new field structure populated with the provided data."
  [data]
  (struct-map field-struct :content (convert-values data)))

(defn add-segment
  "Adds the provided segment structure to the provided message
  structure and returns a new message."
  [message segment]
  (assoc message :segments (conj (:segments message) segment)))

(defn add-field
  "Adds the provided field structure to the provided segment structure
  and returns a new segment."
  [segment field]
  (assoc segment :fields (conj (:fields segment) field)))

(defn add-fields
  "Adds the provided field structures to the provided segment
  structure and returns a new segment."
  [segment fields]
  (assoc segment :fields (into (:fields segment) fields)))

;;
;; Parser methods
;;

(defmulti get-reader
  "Returns a PushBackReader for the provided Object. We want to wrap
  another Reader but we'll cast to a String and read that if
  required."
  class)

(defmethod get-reader java.io.BufferedReader
  [reader-in] (PushbackReader. reader-in))

(defmethod get-reader java.lang.Readable
  [reader-in] (PushbackReader. reader-in))

(defmethod get-reader :default
  [text-in] (PushbackReader. (StringReader. (apply str text-in))))

(defn- peek-int
  "Returns the next integer that will be read. You can only peek ahead
  one integer."
  [reader]

  (let [next-int (.read reader)]
    (.unread reader next-int)
    next-int))

(defn- expect-char-int
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

(defn- delimiter?
  "Returns true if the provided Integer corresponds to the character
  value of one of this messages delimiters."
  [message int-in]

  (if (= -1 int-in)
    true)

  (if (or (= (:component (:delimiters message)) int-in)
          (= (:repeating (:delimiters message)) int-in)
          (= (:subcomponent (:delimiters message)) int-in)
          (= (:field (:delimiters message)) int-in)
          ;(= (:escape (:delimiters message)) int-in)
          (= SEGMENT-DELIMITER int-in))
    true false))

(defn- read-delimiters
  "Parses through the delimiters and returns a map with those
  delimiters (delimiter-struct)."
  [reader]
  ;; loop through the reader, buffer the message id and build up the delimiters
  (loop [int-in (.read reader) buffer [] segment-id nil delimiters (struct-map delimiters-struct) char-index 0]

    (cond

      (= -1 int-in)
      (throw (Exception. "End of file reached while reading delimiters for segment"))

      (= SEGMENT-DELIMITER int-in)
      (throw (Exception. "End of segment reached while reading delmiters"))

      ;; read the field delimiter
      (= 0 char-index)
      (recur (.read reader) buffer segment-id (assoc delimiters :field int-in) (inc char-index))

      ;; read the component delimiter
      (= 1 char-index)
      (recur (.read reader) buffer segment-id (assoc delimiters :component int-in) (inc char-index))

      ;; read the repeating delimiter
      (= 2 char-index)
      (recur (.read reader) buffer segment-id (assoc delimiters :repeating int-in) (inc char-index))

      ;; read the escape delimiter
      (= 3 char-index)
      (recur (.read reader) buffer segment-id (assoc delimiters :escape int-in) (inc char-index))

      ;; read the subcomponent delimiter
      (= 4 char-index)
      (recur (.read reader) buffer segment-id (assoc delimiters :subcomponent int-in) (inc char-index))

      ;; throw an exception if this isn't a field delimiter
      (= 5 char-index)
      (do
        (if (not (expect-char-int (:field delimiters) int-in))
          (throw (Exception. "Expected beginning of next segment but read more delimiter data")))
        (.unread reader int-in)
        delimiters)

      ;; handle text, this is likely the segment's id
      :else
      (recur (.read reader) (conj buffer (char int-in)) segment-id delimiters (inc char-index)))))

(defn- read-segment-delimiters
  "Parsers through the MSH or FHS segment up until the end of the first field (the
  list of delimiters) and returns a map with the segment id (:segment-id) and
  the the delimiter values (:delimiters with the delimiter-struct)."
  [reader]

  ;; loop through the reader, buffer the message id and build up the delimiters
  (loop [int-in (.read reader) buffer [] segment-id nil delimiters (struct-map delimiters-struct) char-index 0]

    (cond

      (= -1 int-in)
      (throw (Exception. "End of file reached while reading MSH or FHS segment"))

      (= SEGMENT-DELIMITER int-in)
      (throw (Exception. "End of segment reached while reading MSH or FHS segment"))

      ;; after reading 3 characters, make sure this is an MSH segment
      ;; and then start reading the delimiters
      (= 3 char-index)
      (let [segment-id (apply str buffer)]
        (if (not (or (= "MSH" segment-id)
                     (= "FHS" segment-id)))
          (throw (Exception. (str "Expected first segment to have the id of "
                                  "\"MSH\" or \"FHS\"  but found \""
                                  segment-id "\""))))
        (.unread reader int-in)
        {:segment-id segment-id
         :delimiters (read-delimiters reader)})

      ;; handle text, this is likely the segment's id
      :else
      (recur (.read reader) (conj buffer (char int-in)) segment-id delimiters (inc char-index)))))

(defn- read-escaped-text
  "Reads in escaped text to the next escape delimiter character."
  [message reader]

  ;; make sure the next character is an escape delimiter
  (expect-char-int (:escape (:delimiters message)) (.read reader))

  ;; loop through the reader and store the escaped text in the
  ;; buffer. Start the buffer out with the escape delimiter.
  (loop [int-in (.read reader) buffer [(char (:escape (:delimiters message)))]]

    (cond

      (= int-in -1)
      (throw (Exception. "End of data reached while reading escaped text"))

      ;; when we hit the escape delimiter, that's the end of the
      ;; escaped text
      (= (:escape (:delimiters message)) int-in)
      (apply str (conj buffer (char int-in)))

      :else
      (recur (.read reader) (conj buffer (char int-in))))))

(defn- read-text
  "Reads in text up to the next delimiter character."
  [message reader]

  ;; loop the reader and store the text in buffer
  (loop [int-in (.read reader) buffer []]

    (cond

      (= int-in -1)
      (throw (Exception. "End of data reached while reading text"))

      ;; we may encounter some escaped text
      ;; (= (:escape (:delimiters message)) int-in)
      ;; (do (.unread reader int-in)
      ;;     (recur nil (conj buffer (read-escaped-text message reader))))

      ;; if we hit a delimiter, push it back and return the text
      (delimiter? message int-in)
      (do (.unread reader int-in)
          (apply str buffer))

      (= nil int-in)
      (recur (.read reader) buffer)

      ;; store the text in the buffer and read the next int
      :else
      (recur (.read reader) (conj buffer (char int-in))))))


(defn- read-subcomponents
  "Reads in the field subcomponent data from the reader."
  [reader message data]

  ;; make sure the next character is a subcomponent delimiter
  (expect-char-int (:subcomponent (:delimiters message)) (.read reader))

  ;; loop the reader, build up vector of subcomponents by building up
  ;; each subcomponent
  (loop [int-in (.read reader)
         subcomponents (if (not (nil? data)) [data] [])
         subcomponent []]

    (cond

      ;; subcomponent delimiter, add our subcomponent to our vector of
      ;; subcomponents
      (= (:subcomponent (:delimiters message)) int-in)
      (recur (.read reader) (conj subcomponents (apply str subcomponent)) [])

      ;; (= (:escape (:delimiters message)) int-in)
      ;; (do (.unread reader int-in)
      ;;     (recur nil subcomponents (conj subcomponent
      ;;                                    (read-escaped-text message reader))))

      ;; another delimiter type, add our last subcomponent and return
      ;; our vector of subcomponents
      (or (= SEGMENT-DELIMITER int-in)
          (= (:field (:delimiters message)) int-in)
          (= (:component (:delimiters message)) int-in)
          (= (:repeating (:delimiters message)) int-in))
      (do (.unread reader int-in)
          (conj subcomponents (apply str subcomponent)))

      (= nil int-in)
      (recur (.read reader) subcomponents subcomponent)

      ;; build up the individual subcomponent
      :else
      (recur (.read reader) subcomponents (conj subcomponent (char int-in))))))

(defn- read-field
  "Reads in the next field of segment data from the reader. The
  repeating flag indicates that repeating fields are okay, if the flag
  is set to false then repeating fields will be treated the same as
  regular fields. For instance, when parsing a message the repeating
  flag should be set to true. When parsing the individual fields in a
  repeating field, be sure this flag is set to false to ensure
  accurate decoding."
  [reader message repeating]

  ;; throw an exception if we aren't starting with a field or
  ;; repeating delimiter
  (let [int-in (.read reader)]
    (if (not (or (= (:field (:delimiters message)) int-in)
                 (= (:repeating (:delimiters message)) int-in)))
      (throw (Exception. "Expected a field or repeating delimiter when reading field data"))))

  ;; loop through the reader, build up a vector of fields by building
  ;; up each individual field
  (loop [int-in (.read reader) field-data [] current-field nil]

    (cond

      ;; handle repeating fields by recursively calling this function
      (and (= (:repeating (:delimiters message)) int-in) repeating)
      (do (.unread reader int-in)
          (recur nil

                 ;; decide if the current field of data should be
                 ;; added to the last map of repeating field data
                 (let [repeating-data
                       (if (not (map? (first field-data)))
                         [(create-field (if (not (nil? current-field))
                                          (conj field-data (apply str current-field))
                                          field-data))]
                         field-data)]
                   (conj repeating-data (read-field reader message false)))
                 []))

      ;; handle subcomponents, add the current field to our field data
      ;; if it's not nil
      (= (:subcomponent (:delimiters message)) int-in)
      (do (.unread reader int-in)
          (recur nil
                 (conj field-data (read-subcomponents
                                   reader message
                                   (if (not (nil? current-field))
                                     (apply str current-field)
                                     nil)))
                 nil))

      ;; handle components, add the field data to our current data or
      ;; a placeholder component if it's nil
      (= (:component (:delimiters message)) int-in)
      (recur (.read reader)
             (if (not (nil? current-field))
               (conj field-data (apply str current-field))
               (if (> 1 (count field-data))
                 [""]
                 field-data))
             [""])

      ;; handle the end of the field or segment by returning our field
      ;; data
      (or (= SEGMENT-DELIMITER int-in)
          (= (:field (:delimiters message)) int-in)
          (and (not repeating) (= (:repeating (:delimiters message)) int-in))
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

      ;; (= (:escape (:delimiters message)) int-in)
      ;; (do (.unread reader int-in)
      ;;     (recur nil field-data (if (not (nil? current-field))
      ;;                             (conj current-field (read-escaped-text message reader))
      ;;                             [(read-escaped-text message reader)])))

      ;; build up the data for our current field
      :else
      (recur (.read reader) field-data
             (if int-in

               ;; if the current field is nil, start a new vector of
               ;; data
               (if (not (nil? current-field))
                 (conj current-field (char int-in)) [(char int-in)])
               current-field)))))

(defn- read-msh-fhs-segment
  "Adds the \"MSH\" or \"BHS\" segment and its first field of data to the provided
  message-struct and returns the new message. This first field will be the list
  of delimiters, the provided message must already have a valid set of
  delimiters."
  [segment-id reader message]

  ;; instantiate our new MSH segment and fill the first field with our
  ;; delimiters
  (let [segment (add-field (create-segment segment-id)
                           (create-field (pr-delimiters (:delimiters message))))]

    ;; loop through the reader and build up our fields
    (loop [int-in (.read reader) fields []]

      (cond

        (= -1 int-in)
        (do
          (println (pr-str message))
          (println (pr-str segment))
          (println (pr-str fields))
          (throw (Exception. "End of file reached while reading segment data")))

        ;; handle the end of field by reading the next field
        (= (:field (:delimiters message)) int-in)
        (do (.unread reader int-in)
            (recur nil (conj fields (read-field reader message true))))

        ;; handle the end of segement by adding the fields to the
        ;; segment and then returning our segment of data
        (= SEGMENT-DELIMITER int-in)
        (do (.unread reader int-in)
            (add-segment message (add-fields segment fields)))

        ;; keep reading in more field data
        :else
        (recur (.read reader) fields)))))

(defn- read-segment
  "Reads in the segment of data from the reader and returns a new
  message with the segment appended. Note that this method cannot
  handle an MSH segment, it will fail while reading the delimiters in
  the first field of the MSH segment."
  [reader message]

  ;; make sure the next character is a segment delimiter
  (expect-char-int SEGMENT-DELIMITER (.read reader))

  ;; read in our segment id
  (let [segment-id (string/trim (read-text message reader))]

    ;; throw an exception if we don't get a valid segment id
    (if (or (nil? segment-id) (not= 3 (count segment-id)))
      (throw (Exception. (str "Illegal segment id \"" segment-id "\" read"))))

    ;; create our new segment
    (let [segment (if (= "BHS" segment-id)
                    (add-field (create-segment segment-id)
                               (create-field (pr-delimiters (read-delimiters reader))))
                    (create-segment segment-id))]

      ;; loop through the reader and build up the fields for our
      ;; segment
      (loop [int-in (.read reader) fields []]

        (cond

          (= -1 int-in)
          (add-segment message (add-fields segment fields))

          ;; handle segment delimiters by adding our fields to our
          ;; segment and then adding our segment to the message
          (= SEGMENT-DELIMITER int-in)
          (do (.unread reader int-in)
              (add-segment message (add-fields segment fields)))

          ;; handle the field delimiter by reading the next field and
          ;; adding it to our vector of fields
          (= (:field (:delimiters message)) int-in)
          (do (.unread reader int-in)
              (recur nil (conj fields (read-field reader message true))))

          ;; read in more field data
          :else
          (recur (.read reader) fields))))))

(defn- parse-message
  "Parsers the data read by the reader into a valid HL7 message data
  structure (message-struct)."
  [reader]

  ;; loop through the reader and parse the delimiters, the MSH segment
  ;; and them the segments; build up the message structure
  (loop [int-in (.read reader) parsing :delimiters segment-id nil message (create-empty-message)]

    (cond

      ;; handle the end-of-file by returning our message
      (or (= -1 int-in)
          (and (nil? int-in) (= -1 (peek-int reader))))
      message

      ;; parse out the delimiters, then loop to get the MSH segment
      (= parsing :delimiters)
      (do (.unread reader int-in)
          (let [delimiters (read-segment-delimiters reader)]
            (recur nil :header-segment
                   (:segment-id delimiters)
                   (assoc message :delimiters (:delimiters delimiters)))))

      ;; parse out the header (MSH or FHS) segment then loop for the other segments
      (= parsing :header-segment)
      (recur nil :segment segment-id (read-msh-fhs-segment segment-id reader message))

      ;; parse out a segment of data and add it to the message
      (= parsing :segment)
      (recur nil :segment segment-id (read-segment reader message))

      ;; loop to read more of the message
      :else
      (recur (.read reader) parsing segment-id message))))

(defn parse
  "Reads data from the provided source (a Reader, String, etc.) and
  parses that data into a hash-map (hl7-struct) that represents the
  content of the message."
  [message-source]
  (parse-message (get-reader message-source)))
