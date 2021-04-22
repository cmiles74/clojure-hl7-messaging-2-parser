(ns com.nervestaple.hl7-parser.batch
  (:require
   [clojure.java.io :as io]
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
  "Addes the supplied string to the given sequence only if it is not blank."
  [seq item]
  (if (not (string/blank? item))
    (conj seq item)
    seq))

(defn read-message
  "Reads lines from the provided sequence and accumulates one HL7 message.
  Returns a vector where the first item is the message and the second the
  remaining lines in the sequence (or nil if all lines have been read)."
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

      ;; accumulate the current message
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
  "Reads through a set of data and returns a lazy sequence of parsed HL7
  messaging messages. If a sequence is provided it will be read directly
  otherwise a reader will be opened on the provided data."
  ([data-in]
   (cond
     (coll? data-in)
     (let [[message lines] (read-message data-in)]
       (read-messages message lines))

     :else
     (let [lines (get-lines data-in)]
       (read-messages lines))))
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

(defn filter-segment
  "Returns a lazy sequence of messages that have a matching segment id in their
  first segment."
  [segment-id parsed-messages]
  (filter #(= segment-id (:id (first (:segments %)))) parsed-messages))


