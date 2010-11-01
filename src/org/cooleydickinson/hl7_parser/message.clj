;;
;; Functions to make it easier to work with parsed HL7 messages.
;;
(ns org.cooleydickinson.hl7-parser.message)

(defn segment-ids
  "Returns a list of the segment ids present in the message."
  [message]
  (map (fn [segment] (:id segment)) (:segments message)))

(defn get-segments
  "Returns all of the segments in the message that have the provided
  segment id."
  [message segment-id]
  (filter (fn [segment] (= segment-id (:id segment)))
          (:segments message)))

(defn get-segment-field
  "Returns the field with the provided index from the given
  segment. Keep in mind that this function expects the index to adhere
  to the HL7 specification where the first field of data is located at
  index 1. Another gotcha it the MSH segment, it's first field of data
  starts at index 2 and that's the list of delimiters..

  This function will return the id of the segment if you ask for index
  0. For the MSH segment, it will return nil for index 1 instead of
  returning the field delimiter. If you want the field delimiter you
  can get it under the :delimiter key of the message."
  [segment index]

  (cond

    ;; handle MSH differently
    (= "MSH" (:id segment))
    (cond

      ;; index 0 returns the segment id
      (= 0 index)
      (:id segment)

      ;; index 1 should return the field delimiter
      (= 1 index)
      nil

      ;; correct our index and return the field
      :else
      (let [real-index (- index 2)]
        (nth (:fields segment) real-index)))
    
    :else
    (cond

      ;; index 0 returns the segment id
      (= 0 index)
      (:id segment)

      ;; correct our index and return the field
      :else
      (let [real-index (dec index)
            field (nth (:fields segment) real-index)]
        (if (map? field)
          (:content field)
          field)))))

(defn get-field
  "Returns the field with the provided index from the segment with the
  given id of the provided message."
  [message segment-id field-index]
  (map (fn [segment] (get-segment-field segment field-index))
       (get-segments message segment-id)))

(defn get-field-component
  "Returns the component at the provided index from the field with the
provided index from the segment with the given id in the provided
message."
  [message segment-id field-index component-index]
  (apply (fn [results]
           (if (list? (first results))
             (first results)
             results))
         
         (map (fn [field-data]

                ;; handle repeating fields
                (if (map? (first field-data))
                  (for [field-in field-data]
                    (nth (:content field-in) component-index))

                  ;; return the component
                  (nth field-data component-index)))

              ;; select matching segments
              (map (fn [segment]
                     (get-segment-field segment field-index))
                   (get-segments message segment-id)))))