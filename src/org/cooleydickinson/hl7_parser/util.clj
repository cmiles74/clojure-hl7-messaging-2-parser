;;
;; Provides utility methods for working with parsed HL7 Messages.
;;
(ns org.cooleydickinson.hl7-parser.util)

(defn dump-collection
  [coll]
  (str "\"" (apply str (interpose "\", \"" coll)) "\""))

(defn dump-field
  [field]

  (let [items (count (:content field))]

    (cond

      (= 0 items)
      "null"

      (= 1 items)
      (if (map? (first (:content field)))
        (str "(Repeating)  " (dump-field (first (:content field))))
        (str "(Atom)       \"" (first (:content field)) "\""))

      (map? (first (:content field)))
      (str "(Repeating)  "
           (apply str (interpose "\n                    "
                                 (map dump-field (:content field)))))

      :else
      (str "(Component)  "
           (apply str
                  (interpose ", "
                             (map (fn [item]
                                    (if (coll? item)
                                      (str " (Subcomponent) " (dump-collection item))
                                      (str "\"" item "\"")))
                                  (:content field))))))))

(defn dump-segment
  [segment]

  (println (str "ID: " (:id segment)))

  (loop [field (first (:fields segment)) fields (rest (:fields segment)) index 0]

    (println (str "  "
                  (if (> 10 index)
                    (str " " index) index)
                  ":  " (dump-field field)))
    
    (if (seq fields)
      (recur (first fields) (rest fields) (inc index)))))

(defn dump-delimiters
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
  [parsed-message]

  (dump-delimiters (:delimiters parsed-message))
  (println)

  (doseq [segment (:segments parsed-message)]
    (dump-segment segment)
    (println)))