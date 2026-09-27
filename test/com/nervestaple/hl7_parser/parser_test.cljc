(ns com.nervestaple.hl7-parser.parser-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [com.nervestaple.hl7-parser.parser :as sut]
   [com.nervestaple.hl7-parser.sample-message :as sample]
   [com.nervestaple.hl7-parser.parser :as parser]))

(def short-message-parsed
  {:delimiters
   {:field 124, :component 94, :subcomponent 38, :repeating 126, :escape 92},
   :segments
   [{:id "MSH",
     :fields
     [{:content ["^~\\&"]}
      {:content ["AcmeHIS"]}
      {:content ["StJohn"]}
      {:content ["CATH"]}
      {:content ["StJohn"]}
      {:content ["20061019172719"]}
      {:content []}
      {:content ["ORM" "O01"]}
      {:content ["1676735383748"]}
      {:content ["P"]}
      {:content ["2.3"]}]}
    {:id "PID",
     :fields
     [{:content []}
      {:content []}
      {:content ["20301"]}
      {:content []}
      {:content ["Durden" "Tyler" "" "" "Mr."]}
      {:content []}
      {:content ["19700312"]}
      {:content ["M"]}
      {:content []}
      {:content []}
      {:content ["88 Punchward Dr." "" "Los Angeles" "CA" "11221" "USA"]}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}]}
    {:id "PV1",
     :fields
     [{:content []}
      {:content ["O"]}
      {:content ["OP" "" ""]}
      {:content []}
      {:content []}
      {:content []}
      {:content ["4652" "Paulson" "Robert"]}
      {:content []}
      {:content []}
      {:content ["OP"]}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content ["9"]}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content []}
      {:content ["20061019172717"]}
      {:content ["20061019172718"]}]}
    {:id "ORC", :fields [{:content ["NW"]} {:content ["20061019172719"]}]}
    {:id "OBR",
     :fields
     [{:content ["1"]}
      {:content ["20061019172719"]}
      {:content []}
      {:content ["76770" "Ultrasound: retroperitoneal" "C4"]}
      {:content []}
      {:content []}
      {:content ["12349876"]}]}]})

(deftest parse-message-test
  (testing "Parses a test message"
    (is (= short-message-parsed (sut/parse (sample/message))))))

(deftest parse-message-test-no-trailing-segment-delimiter
  (testing "Parses a test message"
    (is (= short-message-parsed (sut/parse (string/trim (sample/message)))))))

(deftest parse-message-test-extra-trailing-segment-delimiter
  (testing "Parses a test message"
    (is (thrown? #?(:clj Exception :cljs js/Error) (sut/parse (str (sample/message) (char parser/ASCII_CR)))))))

(deftest emit-message-test
  (testing "Emits the test message"
    (is (= (sample/message) (parser/str-message short-message-parsed)))))

(deftest empty-message-delimiters-test
  (testing "Empty messages contain default delimiters"
    (is (= (:delimiters short-message-parsed)
           (:delimiters (parser/create-empty-message))))))

(deftest pr-delimiters-test
  (testing "Returns a string with the provided delimiters"
    (is (= "^~\\&"
           (parser/pr-delimiters parser/DEFAULT-DELIMITERS)))))

(deftest create-empty-message
  (testing "Creates an empty message"
    (is (= {:delimiters
            {:field 124, :component 94, :subcomponent 38, :repeating 126, :escape 92},
            :segments []}
           (parser/create-empty-message)))))

(deftest create-empty-message-with-delimiters
  (testing "Creates an empty message"
    (is (= {:delimiters
            {:field 1 :component 2 :subcomponent 3 :repeating 4 :escape 5}
            :segments []}
           (parser/create-empty-message
            {:field 1 :component 2 :subcomponent 3 :repeating 4 :escape 5})))))

(deftest create-segment-test
  (testing "Creates an empty segment"
    (is (= {:id "PID" :fields []}
           (parser/create-segment "PID")))))

(deftest create-segment-with-fields
  (testing "Creates a segment with field data"
    (is (= {:id "PID",
            :fields
            [{:content []}
             {:content []}
             {:content ["20301"]}
             {:content []}
             {:content ["Durden" "Tyler" "" "" "Mr."]}
             {:content []}
             {:content ["19700312"]}
             {:content ["M"]}
             {:content []}
             {:content []}
             {:content ["88 Punchward Dr." "" "Los Angeles" "CA" "11221" "USA"]}]}
           (parser/create-segment "PID"
                                  (parser/create-field)
                                  (parser/create-field)
                                  (parser/create-field "20301")
                                  (parser/create-field)
                                  (parser/create-field ["Durden" "Tyler" nil nil "Mr."])
                                  (parser/create-field)
                                  (parser/create-field "19700312")
                                  (parser/create-field "M")
                                  (parser/create-field)
                                  (parser/create-field)
                                  (parser/create-field ["88 Punchward Dr." nil "Los Angeles" "CA" "11221" "USA"]))))))

(deftest add-segment-to-message
  (testing "Creates a message and adds as segment"
    (is (= {:delimiters
            {:field 124, :component 94, :subcomponent 38, :repeating 126, :escape 92},
            :segments
            [{:id "PID",
              :fields
              [{:content []}
               {:content []}
               {:content ["20301"]}
               {:content []}
               {:content ["Durden" "Tyler" "" "" "Mr."]}
               {:content []}
               {:content ["19700312"]}
               {:content ["M"]}
               {:content []}
               {:content []}
               {:content ["88 Punchward Dr." "" "Los Angeles" "CA" "11221" "USA"]}]}]}
           (parser/add-segment
            (parser/create-empty-message)
            (parser/create-segment "PID"
                                   (parser/create-field)
                                   (parser/create-field)
                                   (parser/create-field "20301")
                                   (parser/create-field)
                                   (parser/create-field ["Durden" "Tyler" nil nil "Mr."])
                                   (parser/create-field)
                                   (parser/create-field "19700312")
                                   (parser/create-field "M")
                                   (parser/create-field)
                                   (parser/create-field)
                                   (parser/create-field ["88 Punchward Dr." nil "Los Angeles" "CA" "11221" "USA"])))))))

(deftest add-field-to-segment
  (testing "Adds a field to a segment"
    (is (= {:id "PID",
            :fields
            [{:content []}
             {:content []}
             {:content ["20301"]}
             {:content []}
             {:content ["Durden" "Tyler" "" "" "Mr."]}]}
           (parser/add-field
            (parser/create-segment "PID"
                                   (parser/create-field)
                                   (parser/create-field)
                                   (parser/create-field "20301")
                                   (parser/create-field))
            (parser/create-field ["Durden" "Tyler" nil nil "Mr."]))))))

(deftest add-fields-to-segment
  (testing "Adds a field to a segment"
    (is (= {:id "PID",
            :fields
            [{:content []}
             {:content []}
             {:content ["20301"]}
             {:content []}
             {:content ["Durden" "Tyler" "" "" "Mr."]}]}
           (parser/add-fields
            (parser/create-segment "PID"
                                   (parser/create-field)
                                   (parser/create-field)
                                   (parser/create-field "20301"))
            [(parser/create-field)
             (parser/create-field ["Durden" "Tyler" nil nil "Mr."])])))))

(defn- local-date
  "Returns a date for the provided local time, the month is 1 to 12."
  [year month day hour minute second]
  #?(:clj (java.util.Date. (- year 1900) (dec month) day hour minute second)
     :cljs (js/Date. year (dec month) day hour minute second)))

(deftest format-timestamp-test
  (testing "Formats a date as an HL7 timestamp in local time"
    (is (= "20240105070809" (parser/format-timestamp (local-date 2024 1 5 7 8 9)))))
  (testing "Formats the current time when no date is provided"
    (is (re-matches #"\d{14}" (parser/format-timestamp)))))

(deftest pr-field-with-dates-test
  (testing "Dates in a field are emitted as HL7 timestamps"
    (is (= "20240105070809^20241231235958&x"
           (parser/pr-field parser/DEFAULT-DELIMITERS
                            (parser/create-field [(local-date 2024 1 5 7 8 9)
                                                  [(local-date 2024 12 31 23 59 58) "x"]]))))))

#?(:clj
   (deftest format-timestamp-concurrent-test
     (testing "Formats dates correctly when called from several threads"
       (let [dates (mapv #(java.util.Date. (+ 1000000000000 (* % 86400123))) (range 1000))
             formatter (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
             expected (mapv #(.format formatter (java.time.LocalDateTime/ofInstant
                                                 (.toInstant ^java.util.Date %)
                                                 (java.time.ZoneId/systemDefault)))
                            dates)
             results (->> (range 8)
                          (mapv (fn [_] (future (mapv parser/format-timestamp dates))))
                          (mapv deref))]
         (is (every? #(= expected %) results))))))

(deftest pr-segment-test
  (testing "Emits header segments with the delimiters as the first field"
    (is (= "MSH|^~\\&|A|B^C"
           (parser/pr-segment parser/DEFAULT-DELIMITERS
                              (parser/create-segment "MSH" (parser/create-field "^~\\&")
                                                     (parser/create-field "A")
                                                     (parser/create-field ["B" "C"])))))
    (is (= "MSH|^~\\&"
           (parser/pr-segment parser/DEFAULT-DELIMITERS
                              (parser/create-segment "MSH" (parser/create-field "^~\\&"))))))
  (testing "Emits FHS and BHS segments"
    (let [text "FHS|^~\\&|APP|FAC\rBHS|^~\\&|APP|FAC\rMSH|^~\\&|A|B\rPID|1\r"]
      (is (= text (parser/str-message (parser/parse text)))))))

(deftest parse-message-without-trailing-segment-delimiter-test
  (testing "Parses a message with only an MSH segment and no trailing segment delimiter"
    (is (= {:delimiters
            {:field 124, :component 94, :repeating 126, :escape 92, :subcomponent 38}
            :segments
            [{:id "MSH" :fields [{:content ["^~\\&"]} {:content ["A"]} {:content ["B" "C"]}]}]}
           (parser/parse "MSH|^~\\&|A|B^C"))))
  (testing "Parses a message that ends with a segment id and no trailing segment delimiter"
    (is (= [{:id "ZZZ" :fields []} {:id "ZZY" :fields []}]
           (rest (:segments (parser/parse "MSH|^~\\&|A\rZZZ\rZZY")))))))

(deftest parse-message-end-of-segment-in-delimiters-test
  (testing "Throws when the segment ends while reading the delimiters"
    (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                          #"End of segment reached while reading delimiters"
                          (parser/parse "MSH|^~\r")))))

(deftest parse-subcomponents-without-trailing-segment-delimiter-test
  (let [header "MSH|^~\\&|A|B\r"
        fields (fn [text] (-> (parser/parse text) :segments second :fields))]
    (testing "Parses a last field that ends with subcomponents"
      (is (= [{:content ["1"]} {:content []} {:content ["123" "" "" ["HOSP" "1.2.3" "ISO"]]}]
             (fields (str header "PID|1||123^^^HOSP&1.2.3&ISO")))))
    (testing "Parses a last component that ends with a subcomponent"
      (is (= [{:content ["1"]} {:content []} {:content ["A" ["B" "C"]]}]
             (fields (str header "PID|1||A^B&C")))))
    (testing "Parses a last field that ends with a repeat"
      (is (= [{:content ["1"]} {:content []} {:content [{:content ["A"]} {:content ["B"]}]}]
             (fields (str header "PID|1||A~B")))))
    (testing "Parses a last field that ends with a subcomponent delimiter"
      (is (= [{:content ["1"]} {:content []} {:content [["X" ""]]}]
             (fields (str header "PID|1||X&")))))
    (testing "Parses the same message with and without a trailing segment delimiter"
      (doseq [segment ["PID|1||123^^^HOSP&1.2.3&ISO" "PID|1||A^B&C" "PID|1||A~B" "PID|1||X&"]]
        (is (= (fields (str header segment "\r"))
               (fields (str header segment))))))))

(deftest segment-id-whitespace-test
  (let [segment-id (fn [segment]
                     (-> (parser/parse (str "MSH|^~\\&|A|B\r" segment))
                         :segments second :id))]
    (testing "Strips the whitespace that Character/isWhitespace strips"
      (is (= "PID" (segment-id "  PID |1")))
      (is (= "PID" (segment-id "\tPID\t|1")))
      (is (= "PID" (segment-id "PID|1")))
      (is (= "PID" (segment-id " PID |1"))))
    (testing "Keeps the characters that Character/isWhitespace keeps"
      (is (= "PID " (segment-id "PID |1")))
      (is (= "PID" (segment-id "PID|1")))
      (is (= "PID﻿" (segment-id "PID﻿|1"))))))
