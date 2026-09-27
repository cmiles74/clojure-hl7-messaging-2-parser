;;
;; Parses a fixed set of messages and writes the results to a file, so that the
;; output of the JVM and ClojureScript builds can be compared.
;;
;;   lein run -m com.nervestaple.hl7-parser.parity target/parity/jvm.txt
;;   npx shadow-cljs@3.5.1 compile parity && node target/parity/parity.js target/parity/cljs.txt
;;
(ns com.nervestaple.hl7-parser.parity
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [com.nervestaple.hl7-parser.dump :as dump]
   [com.nervestaple.hl7-parser.message :as message]
   [com.nervestaple.hl7-parser.parser :as parser]
   [com.nervestaple.hl7-parser.util :as util]
   #?(:cljs ["fs" :as fs])))

(defn- segments
  "Returns the lines of a message joined with the segment delimiter."
  [& lines]
  (string/join "\r" lines))

(def messages
  "Vector of [name message] pairs."
  [;; from test/com/nervestaple/hl7_parser/sample_message.clj (with a fixed message id)
   ["sample message"
    (str (segments "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1676735383748|P|2.3"
                   "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||"
                   "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718"
                   "ORC|NW|20061019172719"
                   "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876")
         "\r")]
   ["sample message, no ack"
    (str (segments "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1676735383748|P|2.3|||NE"
                   "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||"
                   "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718"
                   "ORC|NW|20061019172719"
                   "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876")
         "\r")]
   ["sample message, long segment id"
    (str (segments "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1676735383748|P|2.3"
                   "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||"
                   "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718"
                   "ORC|NW|20061019172719"
                   "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"
                   "ZQRY|Y|Y|||||||||||||20230915|000072816|1907838|||||||||")
         "\r")]

   ;; from the clinical-health-message-toolkit message viewer samples, all of
   ;; the patient data is fictitious
   ["ADT^A01 admit"
    (segments "MSH|^~\\&|EPICADT|GOODHEALTH|LABADT|GOODHEALTH|202403011226||ADT^A01^ADT_A01|HL7MSG00001|P|2.5.1"
              "EVN|A01|202403011223"
              "PID|1||MRN12345^5^M11^GOODHEALTH^MR~123456789^^^USSSA^SS||Everyman^Adam^A^III^Mr.||19610615|M||2106-3^White^CDCREC|2222 Home Street^^Greensboro^NC^27401-1020^USA^H||^PRN^PH^^1^555^5552004|||S||PATID12345001^2^M10^GOODHEALTH^AN|444-33-3333"
              "PD1|||Good Health Clinic^^1234|004777^Attend^Aaron^A^^Dr."
              "NK1|1|Everyman^Eve^E|SPO^Spouse^HL70063|2222 Home Street^^Greensboro^NC^27401-1020^USA|^PRN^PH^^1^555^5552005"
              "PV1|1|I|2000^2012^01^GOODHEALTH||||004777^Attend^Aaron^A^^Dr.|||SUR||||ADM|A0|||||||||||||||||||||||||202403011220"
              "PV2|||^Chest pain"
              "AL1|1|DA|^Penicillin|MO|Produces hives"
              "DG1|1||R07.9^Chest pain, unspecified^I10||202403011225|A"
              "GT1|1||Everyman^Adam^A^III||2222 Home Street^^Greensboro^NC^27401-1020^USA|^PRN^PH^^1^555^5552004"
              "IN1|1|BCBS001^Blue Cross PPO|BC1|Blue Cross Blue Shield|PO Box 1000^^Raleigh^NC^27601||^WPN^PH^^1^800^5551000|GRP0042|Acme Corp")]
   ["ORU^R01 lab result"
    (segments "MSH|^~\\&|LAB|StJohn|EHR|StJohn|20240312083015-0500||ORU^R01^ORU_R01|MSG20240312-0042|P|2.5.1"
              "PID|1||20301^^^StJohn^MR||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA"
              "PV1|1|O|OP^^"
              "ORC|RE|ORD448811|FIL992211||CM"
              "OBR|1|ORD448811|FIL992211|24331-1^Lipid panel^LN|||20240311071500|||||||||1234^Singer^Marla^^^Dr.||||||20240312081000|||F"
              "OBX|1|NM|2093-3^Cholesterol, total^LN||212|mg/dL|<200|H|||F|||20240311071500"
              "OBX|2|NM|2571-8^Triglycerides^LN||148|mg/dL|<150|N|||F"
              "OBX|3|NM|2085-9^HDL cholesterol^LN||41|mg/dL|>40|N|||F"
              "NTE|1|L|Patient was fasting for 12 hours.")]
   ["VXU^V04 immunization"
    (segments "MSH|^~\\&|MYEHR|CLINIC||IIS|20210415044526-0500||VXU^V04^VXU_V04|NIST-IZ-001|P|2.5.1|||ER|AL|||||Z22^CDCPHINVS"
              "PID|1||PL-4412^^^CLINIC^MR||Paulson^Robert^Q||19651028|M"
              "ORC|RE||IZ-783274^CLINIC"
              "RXA|0|1|20210401|20210401|208^COVID-19, mRNA, LNP-S, PF, 30 mcg/0.3 mL dose^CVX|0.3|mL^mL^UCUM||00^New immunization record^NIP001||||||EW0150|20210731|PFR^Pfizer, Inc^MVX|||CP|A"
              "RXR|C28161^Intramuscular^NCIT|LD^Left Arm^HL70163")]
   ["ACK with error"
    (segments "MSH|^~\\&|ImmTrac24.16|TEXIIS||BURL6343|20210415044526-0500||ACK^V04^ACK|7008167375|P|2.5.1|||NE|NE|||||Z23^CDCPHINVS|TEXIIS|BURL6343"
              "MSA|AE|7008167375"
              "ERR||NK1^^0|101^Required field missing^HL70357|W|4^Invalid value^HL70533|||IEE-519::Warning. NK1 Segment/Responsible person, missing.")]
   ["ORM^O01 with a bad date"
    (segments "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1788025612436|P|2.3"
              "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||"
              "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718"
              "ORC|NW|20061019172719"
              "OBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876"
              "ZXT|1|Custom^Segment|kept as remainder")]

   ;; edge cases
   ["trailing segment delimiter" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID1|P|2.3" "PID|1||X" "")]
   ["extra trailing segment delimiter" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID1|P|2.3" "PID|1||X" "" "")]
   ["trailing ASCII_CR number, as in parser-test" (str (segments "MSH|^~\\&|A|B|||||ADT^A01|ID1|P|2.3" "PID|1||X" "") parser/ASCII_CR)]
   ["CRLF segment delimiters" (string/join "\r\n" ["MSH|^~\\&|A|B|||||ADT^A01|ID1|P|2.3" "PID|1||X" "OBX|1|TX|||Y" ""])]
   ["LF segment delimiters" (string/join "\n" ["MSH|^~\\&|A|B|||||ADT^A01|ID1|P|2.3" "PID|1||X" "OBX|1|TX|||Y" ""])]
   ["custom delimiters" (segments "MSH#!@$%#APP#FAC#####ADT!A01#ID2#P#2.3" "PID#1##A!B%C%D@E!F#G" "")]
   ["components, subcomponents and repeats" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID3|P|2.3" "PID|1||A&B&C^D~E^F&G~~H|^&|&^|~|x~" "ZZ1|&&|^^|~~|&" "")]
   ["escape sequences" (segments "MSH|^~\\&|A|B|||||ORU^R01|ID4|P|2.3" "OBX|1|TX|||Line one\\.br\\Line two \\F\\ pipe \\S\\ caret \\T\\ amp \\R\\ tilde \\E\\ escape \\X0D\\" "")]
   ["non-ASCII text" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID5|P|2.3" "PID|1||M\u00fcller^Jos\u00e9^\u6587\u5b57^\ud83d\ude00||\u00a0\u2028\u0085|\uffff" "")]
   ["control characters in fields" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID6|P|2.3" "NTE|1||tab\there\u0000nul\u0001soh\u007fdel\u000bvt" "")]
   ["segment with only an id" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID7|P|2.3" "ZZZ" "ZZY")]
   ["FHS/BHS batch" (segments "FHS|^~\\&|APP|FAC|||20240101" "BHS|^~\\&|APP|FAC" "MSH|^~\\&|A|B|||||ADT^A01|ID8|P|2.3" "PID|1||X" "BTS|1" "FTS|1" "")]
   ["BHS with other delimiters" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID9|P|2.3" "BHS|#!@$|X" "")]
   ["MLLP framing" (str "\u000b" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID10|P|2.3" "PID|1||X" "") "\u001c\r")]
   ["empty string" ""]
   ["MSH only, no segment delimiter" "MSH|^~\\&|A|B|||||ADT^A01|ID11|P|2.3"]
   ["first segment not MSH" (segments "PID|1||X" "")]
   ["EOF in segment id" "MS"]
   ["EOF in delimiters" "MSH|^~"]
   ["end of segment in delimiters" (segments "MSH|^~" "")]
   ["delimiters too long" (segments "MSH|^~\\&X|A" "")]
   ["short segment id" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID12|P|2.3" "P|1" "")]
   ["whitespace segment id" (segments "MSH|^~\\&|A|B|||||ADT^A01|ID13|P|2.3" "  PID |1" "")]
   ;; the JVM and ClojureScript trim different characters, these ids are trimmed
   ;; with the set that Character/isWhitespace uses
   ["non-breaking space in segment id"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID18|P|2.3" "PID\u00a0|1" "")]
   ["file separator in segment id"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID19|P|2.3" "PID\u001c|1" "")]
   ["byte order mark in segment id"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID20|P|2.3" "PID\ufeff|1" "")]

   ;; the last segment has no segment delimiter, so the data ends inside the
   ;; subcomponents of the last field
   ["subcomponents, no segment delimiter"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID14|P|2.3" "PID|1||123^^^HOSP&1.2.3&ISO")]
   ["component ending in a subcomponent, no segment delimiter"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID15|P|2.3" "PID|1||A^B&C")]
   ["repeat at the end, no segment delimiter"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID17|P|2.3" "PID|1||A~B")]
   ["subcomponent delimiter at the end, no segment delimiter"
    (segments "MSH|^~\\&|A|B|||||ADT^A01|ID16|P|2.3" "PID|1||X&")]])

(defn- attempt
  "Calls f and returns its result, or a map with the error message if it throws."
  [f]
  (try
    (f)
    (catch #?(:clj Exception :cljs :default) e
      {:error (ex-message e)})))

(defn- code-units
  "Returns the UTF-16 code units of the string."
  [text]
  (when text
    #?(:clj (mapv int text)
       :cljs (mapv #(.charCodeAt text %) (range (count text))))))

(defn- normalize-ack
  "Replaces the generated timestamp in the ACK (MSH-7) with a placeholder, after
  checking that it has the HL7 timestamp format."
  [ack]
  (when ack
    (update-in ack [:segments 0 :fields 5 :content 0]
               (fn [timestamp]
                 (if (re-matches #"\d{14}" timestamp)
                   "<timestamp yyyyMMddHHmmss>"
                   (str "<unexpected timestamp " (pr-str timestamp) ">"))))))

(def ack-options
  {:sending-app "Clojure HL7 Parser"
   :sending-facility "Test Facility"
   :production-mode "P"
   :version "2.3"
   :text-message "Message processed successfully"})

(defn- section
  [title value]
  (println (str ";; " title))
  ;; cljs.pprint leaves a trailing space on some wrapped lines, clojure.pprint doesn't
  (print (string/replace (with-out-str (pprint value)) #" +\n" "\n")))

(defn- report-message
  [[name text]]
  (println (str ";;;; " name))
  (section "input" text)
  (let [parsed (attempt #(parser/parse text))]
    (section "parse" parsed)
    (section "parse (seq of characters)" (= parsed (attempt #(parser/parse (seq text)))))
    (section "parse (reader)"
             (= parsed (attempt #(parser/parse #?(:clj (java.io.StringReader. text)
                                                  :cljs text)))))
    (section "parse (buffered reader)"
             (= parsed (attempt #(parser/parse #?(:clj (java.io.BufferedReader. (java.io.StringReader. text))
                                                  :cljs text)))))
    (section "message-id-unparsed" (message/message-id-unparsed text))
    (section "sanitize-message" (util/sanitize-message text))
    (section "sanitize-message code units" (code-units (util/sanitize-message text)))
    (section "ack-message-fallback" (normalize-ack (message/ack-message-fallback ack-options "AR" text)))
    (section "ack-message-fallback, message id"
             (message/ack-message-fallback (assoc ack-options :message-id "ACK1") "AR" text))
    (section "str-message ack-message-fallback, message id"
             (attempt #(parser/str-message
                        (message/ack-message-fallback (assoc ack-options :message-id "ACK1") "AR" text))))
    (when-not (:error parsed)
      (let [ids (message/segment-ids parsed)]
        (section "str-message" (attempt #(parser/str-message parsed)))
        (section "str-message round trip" (= parsed (attempt #(parser/parse (parser/str-message parsed)))))
        (section "pr-message" (attempt #(with-out-str (parser/pr-message parsed))))
        (section "segment-ids" ids)
        (section "get-segment-field" (attempt #(mapv (fn [segment]
                                                        (mapv (fn [index]
                                                                [(message/get-segment-field segment index)
                                                                 (message/get-segment-field-raw segment index)])
                                                              (range 0 6)))
                                                      (:segments parsed))))
        (section "get-field-first MSH 10" (attempt #(message/get-field-first parsed "MSH" 10)))
        (section "get-field-first-value MSH 9" (attempt #(message/get-field-first-value parsed "MSH" 9)))
        (section "get-field PID 5" (attempt #(message/get-field parsed "PID" 5)))
        (section "extract-text-from-segments OBX 5"
                 (attempt #(message/extract-text-from-segments parsed "OBX" 5)))
        (section "extract-text-from-segments OBX 5 newline"
                 (attempt #(message/extract-text-from-segments parsed "OBX" 5 "\n")))
        ;; only when the component exists, the error from nth differs between platforms
        (when (< 1 (count (flatten (message/get-field parsed "PID" 5))))
          (section "get-field-component PID 5 1" (attempt #(message/get-field-component parsed "PID" 5 1))))
        (when (and (some #{"PID"} ids)
                   (every? #(< 4 (count (:fields %))) (message/get-segments parsed "PID")))
          (section "set-field PID 5" (attempt #(parser/str-message
                                                 (message/set-field parsed "PID" 5 ["Singer" "Marla" "" "" "Ms."])))))
        (when (some #{"MSH"} ids)
          (section "set-field MSH 10" (attempt #(parser/str-message (message/set-field parsed "MSH" 10 "NEWID"))))
          (let [ack (attempt #(message/ack-message ack-options "AA" parsed))]
            (section "ack-message" (if (:error ack) ack (normalize-ack ack)))
            (when (and ack (not (:error ack)))
              (section "str-message ack" (parser/str-message (normalize-ack ack)))))
          (section "ack-message, message id"
                   (attempt #(message/ack-message (assoc ack-options :message-id "ACK2") "AE" parsed))))
        (println ";; dump")
        (print (attempt #(with-out-str (dump/dump parsed))))
        (println ";; dump, show nulls")
        (print (attempt #(with-out-str (dump/dump parsed true)))))))
  (println))

(def sanitize-inputs
  [nil
   ""
   "plain text"
   (apply str (map char (range 0 40)))
   "del\u007f c1\u0080\u0085\u009f nbsp\u00a0 ls\u2028 ps\u2029 bom\ufeff emoji\ud83d\ude00 lone\ud800"])

(defn- report
  []
  (doseq [entry messages]
    (report-message entry))
  (println ";;;; sanitize-message")
  (doseq [text sanitize-inputs]
    (section (pr-str (code-units text)) (code-units (util/sanitize-message text))))
  (println)
  (println ";;;; builders")
  (section "create-empty-message" (parser/create-empty-message))
  (section "pr-delimiters" (parser/pr-delimiters parser/DEFAULT-DELIMITERS))
  (section "built message"
           (parser/str-message
            (parser/add-segment
             (parser/create-message parser/DEFAULT-DELIMITERS
                                    (parser/create-segment "MSH" (parser/create-field "^~\\&") (parser/create-field "A")))
             (parser/add-fields
              (parser/add-field (parser/create-segment "PID") (parser/create-field ["Durden" nil "Tyler"]))
              [(parser/create-field) (parser/create-field [["a" "b"] "c"]) (parser/create-field nil)]))))
  (section "pr-field with dates"
           (let [date #?(:clj (java.util.Date. 124 0 5 7 8 9) :cljs (js/Date. 2024 0 5 7 8 9))]
             (attempt #(parser/pr-field parser/DEFAULT-DELIMITERS (parser/create-field [date [date "x"]])))))
  (section "parse nil" (attempt #(parser/parse nil)))
  (section "parse vector of strings" (attempt #(parser/parse ["MSH|^~\\&|A" "\r" "PID|1||X" "\r"]))))

(defn main
  "Writes the report to the file at the provided path."
  [path]
  (let [output (with-out-str (report))]
    #?(:clj (spit path output)
       :cljs (fs/writeFileSync path output))))

#?(:clj
   (defn -main
     [path]
     (main path)
     (shutdown-agents)))
