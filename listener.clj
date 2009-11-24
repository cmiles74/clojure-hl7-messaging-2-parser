;;
;; Provides functions for listening to HL7 messages over a network
;; socket.
;;
(ns org.cooleydickinson.messagehub.listener
  (:use
   [clojure.contrib.logging]
   [clojure.contrib.server-socket])
  (:require
   [redis]
   [org.cooleydickinson.messagehub.parser :as parser])
  (:import
   (java.io InputStreamReader)
   (java.io OutputStreamWriter)
   (java.io BufferedInputStream)
   (java.io BufferedReader)
   (java.nio.charset Charset)
   (java.net Socket)))

;; a test message
(def TEST-MESSAGE "PID|||500515|123121|TEST^PCP^^^^||19490125|F||W|PO BOX 89^^GILBERTVILLE^MA^01031^^|WOR
PV1||O|LB|3|||000298^SILVERSTEIN^SUZY^S^^MD|||LB||||1|||000298^SILVERSTEIN^SUZY^S^^MD|
GT1|1||TEST^PCP^||PO BOX 89^^GILBERTVILLE^MA^01031^^|||19490125|F||P|559-62-0314|||1||
IN1|1||428|HEALTH NEW ENGLAND|ONE MONARCH PLACE^^SPRINGFIELD^MA^01144^^|||||||||||TEST
IN2||559-62-0314|^|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||^
IN1|2||200|SELF PAY AFTER INSURANCE|^^^^^^|||||||||||TEST^PCP^|18|19490125|^^^^^^|||||
IN2||559-62-0314|^|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||^^||18|")

;; our ASCII codes
(def ASCII_VT 11)
(def ASCII_FS 28)
(def ASCII_CR 13)
(def ASCII_LF 10)

(defn sanitize-message
  "Removes all control characters from a message."
  [message]
  (. message replaceAll "\\p{Cntrl}" ""))

(defn ascii-decimal-to-string
  "Converts a string of ASCII decimal values to a String."
  [decimal-data]
  (apply str (map char decimal-data)))

(defn handle-message
  "Processes an HL7 message by appending it to the end of the names queue."
  [message queue]

  ;; open a connection to our redis server and use database #1
  (redis/with-server {:host "cdhintraapp01.cooley-dickinson.org"
                      :port 6379
                      :db 1}

    (do

      ;; push the new message onto the stack
      (redis/rpush queue message)))

  ;; log the fact that the message has been handled
  (info (str "Message Handled: " (sanitize-message message))))

(defn handle-incomplete-message
  "Processes an HL7 message."
  [message]
  (warn (str "Incomplete Message: " (sanitize-message message))))

(defn send-test-message
  "Sends a test message to provided port on the local host."
  [port]
  (let [socket (new Socket "localhost" port)]
    
    (binding [*in* (new BufferedInputStream (. socket getInputStream))
              *out* (new OutputStreamWriter (. socket getOutputStream))]

      (print (char 11))
      (print TEST-MESSAGE)
      (print (char 28))
      (print (char 13))
             
      (flush)
      (. *out* close)
      (. *in* close))

    (. socket close)))

(defn hl7-receive-messages
  "Reads through the input stream and parses out HL7 messages as they
  appear."
  [input-stream output-stream]
  
  ;; bind our streams to standard in and out
  (binding [*in* (new BufferedInputStream input-stream)
            *out* (new OutputStreamWriter output-stream)]

    ;; loop through the incoming data char by char until we see the
    ;; beginning of a new message
    (loop [input (. *in* read)]

      ;; a new message has started
      (if (= ASCII_VT input)
         
        ;; accumulate the content of the message and continue to read
        ;; characters
        (loop [message (vector input)
               message-input (. *in* read)]
          
          ;; only read more characters if we aren't at the end of the
          ;; message or the end of our data
          (if (or (= -1 message-input)
                  (and (= ASCII_CR message-input)
                       (= ASCII_FS (last message))))

            (if (and (= ASCII_CR message-input) (= ASCII_FS (last message)))

              ;; this is the end of the message, handle it
              (handle-message (ascii-decimal-to-string
                               (conj message message-input))
                              "incoming-messages")

              ;; this message didn't complete normally
              (handle-incomplete-message
               (ascii-decimal-to-string
                (conj message message-input))))

            (recur (conj message message-input)
                   (. *in* read))))

        ;; out of band data recieved, ignore end-of-file and line-feed
        ;; markers
        (if (not (or (= -1 input)
                     (= ASCII_LF input)))
          (warn (str "Out-of-band data received: " input))))
      
      ;; as long as there is data to read, keep reading
      (if (not= -1 input)
        (recur (. *in* read))))

    ;; the connection has been closed, flush the socket and exit
    (flush)))

(defn start-server
  "Starts a new server process."
  []

  ;; listen for new HL7 messages
  (create-server 10000 hl7-receive-messages 5))