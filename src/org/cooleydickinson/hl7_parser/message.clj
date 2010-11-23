;;
;; Functions to make it easier to work with parsed HL7 messages.
;;
(ns org.cooleydickinson.hl7-parser.message
  (:use
   [org.cooleydickinson.hl7-parser.parser2]
   [org.cooleydickinson.hl7-parser.util]
   [org.cooleydickinson.hl7-parser.dump]
   [org.cooleydickinson.hl7-parser.message]))

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
            field (if (< real-index (count (:fields segment)))
                      (nth (:fields segment) real-index))]
        (if (map? field)
          (:content field)
          field)))))

(defn get-field
  "Returns the field with the provided index from the segment with the
  given id of the provided message."
  [message segment-id field-index]
  (map (fn [segment] (get-segment-field segment field-index))
       (get-segments message segment-id)))

(defn- get-nth-field
  "Returns the item at index in the collection of field data. If
  passed a collection of fields, the item at index from each field is
  returned. If passed a collection that contains subcomponents, the
  item at index for each subcomponent is returned."
  [index field-or-fields]
  (cond
    (map? (first field-or-fields))
    (map (fn [field]
           (get-nth-field index (:content field)))
         field-or-fields)

    (coll? (first field-or-fields))
    (map (partial get-nth-field index) field-or-fields)

    :else
    (nth field-or-fields index)))

(defn get-field-component
  "Returns the component at the provided index from the field with the
provided index from the segment with the given id in the provided
message."
  [message segment-id field-index component-index]
  (let [data (flatten (get-field message segment-id field-index))]
    (get-nth-field component-index data)))

(defn set-field
  "Updates the message by altering the field value for the specified
  segment. When specifying field indexes, be sure to use the correct
  HL7 index (the segment id would be 0, the first field is at index
  1).

  Your value should be an atom or an collection, a collection
  indicates a field with components. Subcomponents are represented as
  a collection containing a collection. Pass in a collection of fields
  to indicate repeating fields."
  [message segment-id field-index value]

  ;; correct our index and value (put an atom in a collection)
  (let [field-index-fixed (dec field-index)
        field-value (if (coll? value) value [value])]

    ;; throw an error if we have an illegal HL7 index
    (if (< field-index-fixed 0)
      (throw (Exception. "The first field is at index 1")))

    ;; create a whole new message
    (struct-map message-struct
      :delimiters (:delimiters message)

      ;; map over our segments looking for the one we're changing
      :segments (map (fn [segment]
                       
                       (if (= segment-id (:id segment))

                         ;; associate our new fields
                         (assoc segment :field

                                ;; associate our new value with the
                                ;; field collections
                                (assoc (:fields segment)
                                  field-index-fixed
                                  (create-field field-value)))

                         ;; return the segment unaltered
                         segment))

                     (:segments message)))))