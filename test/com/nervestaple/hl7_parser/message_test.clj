(ns com.nervestaple.hl7-parser.message-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   [com.nervestaple.hl7-parser.message :as sut]
   [com.nervestaple.hl7-parser.parser :as parser]
   [com.nervestaple.hl7-parser.sample-message :as sample]))

(deftest get-segment-field-raw-test
  (testing "Gets the field of a segment"
    (is (= ["Durden" "Tyler" "" "" "Mr."]
           (sut/get-segment-field
            (first (sut/get-segments (parser/parse (sample/message)) "PID"))
            5)))))

(deftest get-segment-field-raw-test
  (testing "Gets the raw value for a segment's field"
    (is (= {:content ["Durden" "Tyler" "" "" "Mr."]}
           (sut/get-segment-field-raw
            (first (sut/get-segments (parser/parse (sample/message)) "PID"))
            5)))))

(deftest get-field-test
  (testing "Gets the field of a segment"
    (is (= (list ["Durden" "Tyler" "" "" "Mr."])
           (sut/get-field (parser/parse (sample/message)) "PID" 5)))))

(deftest get-field-component-test
  (testing "Gets the component of a field at the provided index"
    (is (= "Tyler"
           (sut/get-field-component (parser/parse (sample/message)) "PID" 5 1)))))

(deftest set-field-test
  (testing "Sets the value of a field of a parsed message"
    (is (= "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|1676735383748|P|2.3\rPID|||20301||Singer^Marla^^^Ms.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||\rPV1||O|OP^^||||4652^Paulson^Robert|||OP|||||||||9|||||||||||||||||||||||||20061019172717|20061019172718\rORC|NW|20061019172719\rOBR|1|20061019172719||76770^Ultrasound: retroperitoneal^C4|||12349876\r"
           (parser/str-message (sut/set-field (parser/parse (sample/message))
                                              "PID" 5
                                              ["Singer" "Marla" "" "" "Ms."]))))))

(deftest extract-text-test
  (testing "Extracts and concatenates test of the first instance of the field at the index"
    (is (= "DurdenTylerMr."
           (sut/extract-text-from-segments (parser/parse (sample/message))
                                           "PID" 5)))))

(deftest get-field-first-test
  (testing "Fetches the first instance of the field at the index"
    (is (= ["Durden" "Tyler" "" "" "Mr."]
           (sut/get-field-first (parser/parse (sample/message))
                                      "PID" 5)))))

(deftest get-field-first-value-test
  (testing "Fetches the value of the first instance of the field at the index"
    (is (= "Durden^Tyler^^^Mr."
           (sut/get-field-first-value (parser/parse (sample/message))
                                      "PID" 5)))))

(deftest ack-test
  (testing "Generates an acknowledgement for a parsed message"
    (is (= {:delimiters
            {:field 124, :component 94, :subcomponent 38, :repeating 126, :escape 92},
            :segments
            [{:id "MSH",
              :fields
              [{:content ["^~\\&"]}
               {:content [""]}
               {:content [""]}
               {:content ["AcmeHIS"]}
               {:content ["StJohn"]}
               {:content ["20230218125600"]}
               {:content []}
               {:content ["ACK"]}
               {:content ["1676735383748"]}
               {:content [""]}
               {:content [""]}]}
             {:id "MSA",
              :fields [{:content ["AA"]} {:content ["1676735383748"]} {:content [""]}]}]}
           (sut/ack-message {:message-id "20230218125600"}
                            "AA" (parser/parse (sample/message)))))))

(deftest no-ack-test
  (testing "Does not generate an acknowledgement for a parsed message"
    (is (= nil
           (sut/ack-message {} "AA" (parser/parse (sample/message-no-ack)))))))

(deftest ack-fallback-test
  (testing "Generates an acknowledgement for a parsed message"
    (is (= {:delimiters
            {:field 124, :component 94, :subcomponent 38, :repeating 126, :escape 92},
            :segments
            [{:id "MSH",
              :fields
              [{:content ["^~\\&"]}
               {:content [""]}
               {:content [""]}
               {:content ["UNKNOWN"]}
               {:content ["UNKNOWN"]}
               {:content ["20230218125600"]}
               {:content []}
               {:content ["ACK"]}
               nil
               {:content [""]}
               {:content [""]}]}
             {:id "MSA", :fields [{:content ["AR"]} nil {:content [""]}]}]}
           (sut/ack-message-fallback {:message-id "20230218125600"}
                                     "AR" "BLERG!")))))
(deftest message-with-long-segment-id
  (testing "Parses a message that includes a long segment identifier"
    (is  (= {:id "ZQRY"
             :fields [{:content ["Y"]} {:content ["Y"]} {:content []}
                      {:content []} {:content []} {:content []} {:content []}
                      {:content []} {:content []} {:content []} {:content []}
                      {:content []} {:content []} {:content []}
                      {:content ["20230915"]} {:content ["000072816"]}
                      {:content ["1907838"]} {:content []} {:content []}
                      {:content []} {:content []} {:content []} {:content []}
                      {:content []} {:content []} {:content []}]}
            (first (sut/get-segments (parser/parse (sample/message-long-segment-id)) "ZQRY"))))))
