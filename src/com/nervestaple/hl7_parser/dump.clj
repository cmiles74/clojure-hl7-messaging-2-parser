;;
;; Provides functions for displaying human-readable versions of HL7
;; messages.
;;
(ns com.nervestaple.hl7-parser.dump)

(defn dump-collection
  "Returns an human-readable String representing a collection of
  atoms."
  [coll]
  (str "\"" (apply str (interpose "\", \"" coll)) "\""))

(defn dump-field
  "Returns an human-readable String representing the content of the
  field."
  [field]

  (let [items (count (:content field))]

    (cond

      (= 0 items)
      "null"

      (= 1 items)
      (if (map? (first (:content field)))
        (str "Repeating    \n             " (dump-field (first (:content field))))
        (str "Atom         \"" (first (:content field)) "\""))

      (map? (first (:content field)))
      (str "Repeating    \n                  "
           (apply str (interpose "\n                  "
                                 (map dump-field (:content field)))))

      :else
      (str "Component    "
           (apply str
                  (interpose ", "
                             (map (fn [[index item]]
                                    (if (coll? item)
                                      (str " Subcomponent   " (dump-collection item))
                                      (str (inc index) ": \"" item "\"")))
                                  (map-indexed #(vector %1 %2)
                                               (:content field)))))))))

(defn dump-segment
  "Returns an human-readable String representing the content of the segment. If
  the show-nulls parameter is provided and is true, null fields will also be
  displayed."
  ([delimiters segment]
   (dump-segment delimiters segment false))
  ([delimiters segment show-nulls]

  (println (str "Segment ID: " (:id segment)))
  (println "Index HL7 Index   Type        Content")
  (println "----- ---------   ----------- -----------")
  (let [segment-index-start (if (= "MSH" (:id segment)) 2 1)]
    (loop [field (first (:fields segment))
           fields (rest (:fields segment))
           index 0
           segment-index segment-index-start]

      (when (and (= "MSH" (:id segment)) (= 0 index))
        (println (str "   -        1     Atom         \""
                      (char (:field delimiters)) "\"")))

      (when (or (< 0 (count (:content field))) show-nulls)
        (println (str "  "
                      (if (> 10 index)
                        (str " " index) index)
                      "       "
                      (if (> 10 segment-index)
                        (str " " segment-index) segment-index)
                      "     " (dump-field field))))

      (if (seq fields)
        (recur (first fields)
               (rest fields)
               (inc index)
               (inc segment-index)))))))

(defn dump-delimiters
  "Returns a human-readable String representing the message's delimiters."
  [delimiters]

  (println "Delimiters: ")

  (if (:field delimiters)
    (println "  Field:         " (char (:field delimiters))))

  (if (:component delimiters)
    (println "  Component:     " (char (:component delimiters))))

  (if (:repeating delimiters)
    (println "  Repeating:     " (char (:repeating delimiters))))

  (if (:escape delimiters)
    (println "  Escape:        " (char (:escape delimiters))))

  (if (:subcomponent delimiters)
    (println "  Subcomponent:  " (char (:subcomponent delimiters)))))

(defn dump
  "Prints a human-readable version of the HL7 message to the current *out* stream.
  If the show-nulls parameter is provided and is true, null fields will also be
  displayed."
  ([parsed-message]
   (dump parsed-message false))
  ([parsed-message show-nulls]

   (dump-delimiters (:delimiters parsed-message))
   (println)

   (doseq [segment (:segments parsed-message)]
     (dump-segment (:delimiters parsed-message) segment show-nulls)
     (println))))
