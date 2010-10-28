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
  starts at index 2.

  This function will return the id of the segment if you ask for index
  0. For the MSH segment, it will return nil for index 1 and 2 instead
  of returning the field delimiter and the list of delimiters
  respectively. If you want this data, you can get it under
  the :delimiter key of the message."
  [index segment]

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

      ;; index 2 should return the delimiters
      (= 2 index)
      nil

      ;; correct our index and return the field
      :else
      (let [real-index (- index 3)]
        (nth (:fields segment) real-index)))
    
    :else
    (cond

      ;; index 0 returns the segment id
      (= 0 index)
      (:id segment)

      ;; correct our index and return the field
      :else
      (let [real-index (dec index)]
        (nth (:fields segment) real-index)))))

(defn get-field
  "Returns the field with the provided index from the segment with the
  given id of the provided message."
  [message segment-id field-index]
  (map (partial get-segment-field field-index) (get-segments message segment-id)))