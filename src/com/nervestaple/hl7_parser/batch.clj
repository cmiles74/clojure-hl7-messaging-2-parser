(ns com.nervestaple.hl7-parser.batch
  (:require
   [clojure.string :as string]
   [com.nervestaple.hl7-parser.parser :as parser])
  (:import
   (java.io BufferedReader StringReader)))

(defmulti get-lines
  "Returns a sequence of lines for the provided Object. We would prefer to wrap
  another reader but we'll read a string if required."
  class)

(defmethod get-lines java.io.BufferedReader
  [reader] (line-seq reader))

(defmethod get-lines java.lang.Readable
  [reader] (line-seq (BufferedReader. reader)))

(defmethod get-lines :default
  [text] (line-seq (BufferedReader. (StringReader. text))))

(defn conj-not-empty
  "Addes the supplied item to the given sequence only if it is not blank."
  [seq item]
  (if (not (string/blank? item))
    (conj seq item)
    seq))

(defn read-message
  [lines-in]
  (loop [line-this (first lines-in) message [] lines (rest lines-in)]
    (cond

      ;; we're out of lines, return the last line
      (nil? line-this)
      [(apply str (interpose (char parser/ASCII_CR) message))
       nil]

      ;; combine the BTS and FTS segments into one message
      (and (< 0 (count message))
           (and (string/starts-with? (first message) "BTS")
                (string/starts-with? line-this "FTS")))
      (recur (first lines)
             (conj-not-empty message line-this)
             (rest lines))

      ;; if the next line starts a new message, return our messages and the
      ;; rest of our lines
      (and (< 0 (count message))
           (or (string/starts-with? line-this "MSH")
               (string/starts-with? line-this "BTS")
               (string/starts-with? line-this "FTS")))
      [(apply str (interpose (char parser/ASCII_CR) message))
       (cons line-this lines)]

      ;; accumilate the current message
      :else
      (recur (first lines)
             (conj-not-empty message line-this)
             (rest lines)))))

(defn parse-trailer
  "Parses the trailer message into something like an HL7 message."
  [message]
  (let [segments (string/split message #"\r")]
    {:segments
     (into []
           (for [segment segments]
             {:id (apply str (take 3 segment))
              :fields (into []
                            (for [field (string/split
                                         (apply str (drop 4 segment))
                                         #"\|")]
                              {:content [field]}))}))}))

(defn parse-message
  "Parses an HL7 messaging batch message, including trailer messages."
  [message]
  (cond
    (or (string/starts-with? message "BTS")
        (string/starts-with? message "FTS"))
    (parse-trailer message)

    :else
    (parser/parse message)))

(defn read-messages
  "Parses a sequence of lines into a sequence of HL7 messaging messages. If the
  provided sequence has header or trailer messages, those are parsed as well. "
  ([lines-in]
   (let [[message lines] (read-message lines-in)]
     (read-messages message lines)))
  ([message lines]
   (cond
     (nil? message)
     nil

     (nil? lines)
     (cons (parse-message message) (read-messages nil nil))

     :else
     (lazy-seq (cons (parse-message message)
                     (let [[message-next rest-lines] (read-message lines)]
                       (read-messages message-next rest-lines)))))))
