;;
;; Provides functions for parsing HL7 messages.
;;
(ns com.nervestaple.hl7-parser.parser
  #?(:clj
     (:import
      (java.text SimpleDateFormat)
      (java.util Date)
      (java.io PushbackReader StringReader))))

(defn format-timestamp
  "Returns an HL7 compatible timestamp (yyyyMMddHHmmss) in local time for the
  provided date (a java.util.Date or a JavaScript Date), or for the current time
  if no date is provided."
  ([]
   (format-timestamp #?(:clj (Date.) :cljs (js/Date.))))
  ([date]
   #?(:clj (.format (SimpleDateFormat. "yyyyMMddHHmmss") ^Date date)
      :cljs (let [pad #(.padStart (str %1) %2 "0")]
              (str (.getFullYear date) (pad (inc (.getMonth date)) 2) (pad (.getDate date) 2)
                   (pad (.getHours date) 2) (pad (.getMinutes date) 2) (pad (.getSeconds date) 2))))))

(defn- error
  "Returns a new platform exception with the provided message."
  [message]
  #?(:clj (Exception. ^String message)
     :cljs (js/Error. message)))

;; ASCII codes of characters used to delimit and wrap messages
(def ASCII_VT 11)
(def ASCII_FS 28)
(def ASCII_CR 13)
(def ASCII_LF 10)

;; ASCII codes of characters used as default delimiters
(def ASCII_PIPE 124)
(def ASCII_CARAT 94)
(def ASCII_AMPERSAND 38)
(def ASCII_TILDE 126)
(def ASCII_BACKSLASH 92)

;; HL7 Messaging v2.x segment delimiter
(def SEGMENT-DELIMITER ASCII_CR)

;; Default set of message delimiters, these are the most common
(def DEFAULT-DELIMITERS
  {:field ASCII_PIPE
   :component ASCII_CARAT
   :subcomponent ASCII_AMPERSAND
   :repeating ASCII_TILDE
   :escape ASCII_BACKSLASH})

;;
;; Emit methods used to output messages
;;

(defn pr-delimiters
  "Returns an HL7 compatible text representation of the provided
  delimiters."
  [delimiters]
  (str (char (:component delimiters))
       (char (:repeating delimiters))
       (char (:escape delimiters))
       (char (:subcomponent delimiters))))

(defn- do-pr-content
  "Returns an HL7 compatible String representation of the provided
  content atom. Only Date objects are afforded special handling, an
  HL7 compatible timestamp is returned."
  [content]
  (if (instance? #?(:clj java.util.Date :cljs js/Date) content)
    (format-timestamp content)
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

  (str (:id segment) (char (:field delimiters))
       (apply str
              (interpose (char (:field delimiters))
                         (map (partial pr-field delimiters) (:fields segment))))))

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
  "Returns a new, empty message map. if no map of delimiters is provided then the
  default set will be used."
  ([]
   (create-empty-message DEFAULT-DELIMITERS))
  ([delimiters]
  {:delimiters delimiters :segments []}))

(defn create-message
  "Accepts a map of delimiters and segments. Returns a new parsed message using
  the provided delimiters populated with the provided segments."
  [delimiters & segments]
  {:delimiters delimiters
   :segments (if (< 0 (count segments)) (vec segments) [])})

(defn create-segment
  "Returns a new, empty segment map with the provided id."
  [id & fields]
  {:id id :fields (if (< 0 (count fields)) (vec fields) [])})

(defn create-field
  "Returns a new field map populated with the provided data."
  ([]
   (create-field nil))
  ([data]
   {:content (convert-values data)}))

(defn add-segment
  "Adds the provided segment map to the provided message map and returns a new
  message."
  [message segment]
  (assoc message :segments (conj (:segments message) segment)))

(defn add-field
  "Adds the provided field map to the provided segment map and returns a new
  segment."
  [segment field]
  (assoc segment :fields (conj (:fields segment) field)))

(defn add-fields
  "Adds the provided field maps to the provided segment map and returns a new
  segment."
  [segment fields]
  (assoc segment :fields (into (:fields segment) fields)))

;;
;; Parser methods
;;

#?(:cljs
   (deftype StringPushbackReader [text ^:mutable position ^:mutable pushed]
     Object
     (read [_]
       (cond
         (some? pushed) (let [value pushed] (set! pushed nil) value)
         (< position (.-length text)) (let [value (.charCodeAt text position)]
                                        (set! position (inc position))
                                        value)
         :else -1))
     (unread [_ value]
       ;; like java.io.PushbackReader, the value is stored as a char
       (set! pushed (bit-and value 0xFFFF)))))

(defmulti get-reader
  "Returns a pushback reader for the provided Object. We want to wrap
  another Reader but we'll cast to a String and read that if
  required. On ClojureScript the Object is always read as a String."
  #?(:clj class :cljs type))

#?(:clj
   (defmethod get-reader java.io.BufferedReader
     [reader-in] (PushbackReader. reader-in)))

#?(:clj
   (defmethod get-reader java.lang.Readable
     [reader-in] (PushbackReader. reader-in)))

#?(:clj
   (defmethod get-reader :default
     [text-in] (PushbackReader. (StringReader. (apply str text-in))))
   :cljs
   (defmethod get-reader :default
     [text-in] (StringPushbackReader. (apply str text-in) 0 nil)))

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
    (throw (error
             (str "End of file reached while looking for " (char char-expect-int)
                  "(" char-expect-int ")")))
    (if (= char-expect-int int-in)
      true
      (throw (error
               (str "Expected \"" (char char-expect-int) "\" (" char-expect-int
                    ") but read \"" (char int-in) "\" (" int-in ")"))))))

(defn- delimiter?
  "Returns true if the provided Integer corresponds to the character
  value of one of this messages delimiters or is the end of the data (-1)."
  [message int-in]

  (if (or (= -1 int-in)
          (= (:component (:delimiters message)) int-in)
          (= (:repeating (:delimiters message)) int-in)
          (= (:subcomponent (:delimiters message)) int-in)
          (= (:field (:delimiters message)) int-in)
          ;(= (:escape (:delimiters message)) int-in)
          (= SEGMENT-DELIMITER int-in))
    true false))

(defn- read-delimiters
  "Parses through the delimiters and returns a map with those delimiters."
  [reader]

  ;; loop through the reader, buffer the message id and build up the delimiters
  (loop [int-in (.read reader)
         buffer []
         segment-id nil
         delimiters {}
         char-index 0]

    (cond

      (= -1 int-in)
      (throw (error "End of file reached while reading delimiters for segment"))

      (= SEGMENT-DELIMITER int-in)
      (throw (error "End of segment reached while reading delimiters"))

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
        (when (not (expect-char-int (:field delimiters) int-in))
          (throw (error
                   "Expected beginning of next segment but read more delimiter data")))
        (.unread reader int-in)
        delimiters)

      ;; handle text, this is likely the segment's id
      :else
      (recur (.read reader)
             (conj buffer (char int-in))
             segment-id
             delimiters
             (inc char-index)))))

(defn- read-segment-delimiters
  "Parsers through the MSH or FHS segment up until the end of the first field (the
  list of delimiters) and returns a map with the segment id (:segment-id) and
  the the delimiter values (a map)."
  [reader]

  ;; loop through the reader, buffer the message id and build up the delimiters
  (loop [int-in (.read reader)
         buffer []
         segment-id nil
         delimiters {}
         char-index 0]

    (cond

      (= -1 int-in)
      (throw (error "End of file reached while reading MSH or FHS segment"))

      (= SEGMENT-DELIMITER int-in)
      (throw (error "End of segment reached while reading MSH or FHS segment"))

      ;; after reading 3 characters, make sure this is an MSH segment
      ;; and then start reading the delimiters
      (= 3 char-index)
      (let [segment-id (apply str buffer)]
        (when (not (or (= "MSH" segment-id)
                     (= "FHS" segment-id)))
          (throw (error (str "Expected first segment to have the id of "
                                  "\"MSH\" or \"FHS\"  but found \""
                                  segment-id "\""))))
        (.unread reader int-in)
        {:segment-id segment-id
         :delimiters (read-delimiters reader)})

      ;; handle text, this is likely the segment's id
      :else
      (recur (.read reader)
             (conj buffer (char int-in))
             segment-id delimiters
             (inc char-index)))))

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
      (throw (error "End of data reached while reading escaped text"))

      ;; when we hit the escape delimiter, that's the end of the
      ;; escaped text
      (= (:escape (:delimiters message)) int-in)
      (apply str (conj buffer (char int-in)))

      :else
      (recur (.read reader) (conj buffer (char int-in))))))

(def ^:private whitespace
  "The characters that java.lang.Character/isWhitespace treats as whitespace: the
  Unicode space, line and paragraph separators that aren't non-breaking, the tab,
  newline, vertical tab, form feed and carriage return, and the file, group,
  record and unit separators. ClojureScript's trim strips a different set (it
  strips the non-breaking space and the byte order mark, and keeps the file,
  group, record and unit separators), so segment ids are trimmed with this set
  on both platforms.
  "
  (into #{\tab \newline \formfeed \return \space}
        (map char)
        [0x000b 0x001c 0x001d 0x001e 0x001f
         0x1680 0x2000 0x2001 0x2002 0x2003 0x2004 0x2005 0x2006 0x2008 0x2009
         0x200a 0x2028 0x2029 0x205f 0x3000]))

(defn- trim-segment-id
  "Removes the leading and trailing whitespace from a segment id."
  [text]
  (let [whitespace? #(contains? whitespace %)]
    (->> (drop-while whitespace? text)
         (reverse)
         (drop-while whitespace?)
         (reverse)
         (apply str))))

(defn- read-text
  "Reads in text up to the next delimiter character."
  [message reader]

  ;; loop the reader and store the text in buffer
  (loop [int-in (.read reader) buffer []]

    (cond

      ;; the end of the data also ends the text
      (= int-in -1)
      (apply str buffer)

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

      ;; the end of the data ends the subcomponents, there's nothing to unread
      (= -1 int-in)
      (conj subcomponents (apply str subcomponent))

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
    (when-not (or (= (:field (:delimiters message)) int-in)
                  (= (:repeating (:delimiters message)) int-in))
      (throw (error
               "Expected a field or repeating delimiter when reading field data"))))

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
  message map and returns the new message. This first field will be the list of
  delimiters, the provided message must already have a valid set of delimiters."
  [segment-id reader message]

  ;; instantiate our new MSH segment and fill the first field with our
  ;; delimiters
  (let [segment (add-field (create-segment segment-id)
                           (create-field (pr-delimiters (:delimiters message))))]

    ;; loop through the reader and build up our fields
    (loop [int-in (.read reader) fields []]

      (cond

        ;; handle the end of the data by adding the fields to the segment,
        ;; the last segment may not have a segment delimiter
        (= -1 int-in)
        (add-segment message (add-fields segment fields))

        ;; handle the end of field by reading the next field
        (= (:field (:delimiters message)) int-in)
        (do (.unread reader int-in)
            (recur nil (conj fields (read-field reader message true))))

        ;; handle the end of segment by adding the fields to the
        ;; segment and then returning our segment of data
        (= SEGMENT-DELIMITER int-in)
        (add-segment message (add-fields segment fields))

        ;; keep reading in more field data
        :else
        (recur (.read reader) fields)))))

(defn- read-segment
  "Reads in the segment of data from the reader and returns a new
  message with the segment appended. Note that this method cannot
  handle an MSH segment, it will fail while reading the delimiters in
  the first field of the MSH segment."
  [reader message]

  ;; read in our segment id
  (let [segment-id (trim-segment-id (read-text message reader))]

    ;; throw an exception if we don't get a valid segment id
    (when (or (nil? segment-id) (> 3 (count segment-id)))
      (throw (error (str "Illegal segment id \"" segment-id "\" read"))))

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
          (add-segment message (add-fields segment fields))

          ;; handle the field delimiter by reading the next field and
          ;; adding it to our vector of fields
          (= (:field (:delimiters message)) int-in)
          (do (.unread reader int-in)
              (recur nil (conj fields (read-field reader message true))))

          ;; read in more field data
          :else
          (recur (.read reader) fields))))))

(defn- parse-message
  "Parses the data read by the reader into a valid HL7 message data map."
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
  "Reads data from the provided source (a Reader, String, etc.) and parses that
  data into a map that represents the content of the message."
  [message-source]
  (parse-message (get-reader message-source)))
