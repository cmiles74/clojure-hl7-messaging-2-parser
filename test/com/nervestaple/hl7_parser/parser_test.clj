(ns com.nervestaple.hl7-parser.parser-test
  (:require [clojure.test :refer :all]
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

(deftest emit-message-test
  (testing "Emits the test message"
    (is (= (sample/message) (parser/str-message short-message-parsed)))))
