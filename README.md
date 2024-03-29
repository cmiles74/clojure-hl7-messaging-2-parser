Clojure HL7 Version 2.x Message Parser
======================================

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.cmiles74/clojure-hl7-parser.svg)](https://clojars.org/org.clojars.cmiles74/clojure-hl7-parser)

This library provides functions for parsing and manipulating [HL7 version 2.x
Messages][hl7v2].

Usage
-----

To use these functions, first require it in your project.

    (ns com.example.project
      (:require
        [com.nervestaple.hl7-parser.parser :as parser]
        [com.nervestaple.hl7-parser.message :as message]))

The "parser" namespace contains functions for parsing and emitting HL7
version 2.x messages. The "message" namespace contains functions for
pulling specific information out of a parsed message as well as
manipulating messages.

In addition, the "dump" namespace has some functions for displaying a
parsed message in a human-readable format. The "util" namespace has
methods that may come in handy while your developing your HL7
solution. The "test" namespace contains functions to make testing
easier, including a test HL7 message.

### Parse A Message

In this example we have the text of an incoming HL7 version 2.x
message in a var called "text-message" parsing it is as easy as...

    (def parsed-message (parser/parse message))

The parsed message is a hash-map that contains all of the information
contained in the message. It's not a lot of fun to look at.

    {:delimiters {:field 124, :component 94, :subcomponent 38,
                  :repeating 126, :escape 92}, 
     :segments [{:id "MSH", :fields [{:content "^~\\&"} 
                {:content ["AcmeHIS"]} {:content ["StJohn"]}
                {:content ["CATH"]} {:content["StJohn"]}...]}

The "message" namespace provides functions that make it easy to get
the information you need out of this structure.

    user> (message/get-field parsed-message "MSH" 10)
    ({:content ["1291058687937"]})

You probably just want the actual value and not the field from the
parsed data structure.

    user> (message/get-field-first-value parsed-message "MSH" 10)
    "1291058687937"

Take a look at the source code, there are functions for retrieving
whole segments, extracting text and changing the value of fields.

### Creating an Acknowledgment Message

Generating acknowledgment messages is easy, we provide a function in
the message namespace just for this purpose. Pass in the required
options, your acknowledgment status and the parsed message that you
are acknowledging.

    (message/ack-message {:sending-app "MYAPP"
                          :sending-facility "TEST LAB"
                          :production-mode "P"
                          :version "2.3"
                          :text-message "Successfully received"}
                          "AA" parsed-message)

This function will return a parsed message containing the
acknowledgment.

### Creating a Message from Scratch

Creating new HL7 messages is easy. You'll need a set of delimiters,
you can create these manually or retrieve them from a message you have
already parsed.

    user> (:delimiters parsed-message)
    {:field 124, :component 94, :subcomponent 38, :repeating 126,
    :escape 92}

Delimiters are stored by their character code in a hashmap. You can create a map
if you need to setup your own.

    user> {:field 124 :component 94 :subcomponent 38 repeating 126 escape 92}

We have functions for building up messages in the "message" namespace.

    (create-message my-delimiters
      (create-segment "MSH"
          (create-field (parser/pr-delimiters my-delimiters))
          (create-field ["MYAPP"])
          (create-field ["TEST LAB"])
          (create-field ["19202830920"])))

That's not a real message or a complete MSH segment but you get the
idea.

### Emit a Message

Emitting a message is also pretty straightforward.

    user> (parser/pr-message parsed-message)
    MSH|^~\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719|...

You can also emit pieces of a message, like the delimiters or a
segment or field. The field and segment methods require that you also
pass in the delimiters for the message.

    user> (parser/pr-segment my-delimiters pid-segment)
    "PID|||20301||Durden^Tyler^^^Mr.||19700312|M|||..."
    
### Dump a Message

Lastly we provide tools for displaying more detailed information about the
structure and content of a parsed message. Provide a parsed message to the
`dump` function to get detail on the segments and their data.

```
Delimiters: 
  Field:          |
  Component:      ^
  Repeating:      ~
  Escape:         \
  Subcomponent:   &

Segment ID: MSH
Index HL7 Index   Type        Content
----- ---------   ----------- -----------
   -        1     Atom         "|"
   0        2     Atom         "^~\&"
   1        3     Atom         "AcmeHIS"
   2        4     Atom         "StJohn"
   3        5     Atom         "CATH"
   4        6     Atom         "StJohn"
   5        7     Atom         "20061019172719"
   7        9     Component    1: "ORM", 2: "O01"
   8       10     Atom         "1676926150678"
   9       11     Atom         "P"
  10       12     Atom         "2.3"

Segment ID: PID
Index HL7 Index   Type        Content
----- ---------   ----------- -----------
   2        3     Atom         "20301"
   4        5     Component    1: "Durden", 2: "Tyler", 3: "", 4: "", 5: "Mr."
   6        7     Atom         "19700312"
   7        8     Atom         "M"
  10       11     Component    1: "88 Punchward Dr.", 2: "", 3: "Los Angeles", 4: "CA", 5: "11221", 6: "USA"
...
```
    
Development
------------

This project is managed with [Leiningen][lein], more information is available on
the project website. If you're working on a pull request, the important thing is
that you have it installed and on your path. With that out of the way, you can
run the tests with [Kaocha][kaocha]

```shell
$ lein kaocha
```

As long as the tests pass, you are on the right track. 😉

Future Plans
------------

I am actively using this library in several projects and will continue
to work on this code. If you have any suggestions or patches, please
fork this project and send me a pull request. 🙂


[hl7v2]: https://secure.wikimedia.org/wikipedia/en/wiki/Health_Level_7#HL7_version_2.x "HL7 v2 Messaging"
[lein]: https://leiningen.org/ "Leiningen"
[kaocha]: https://github.com/lambdaisland/kaocha "Kaocha Test Runner"
