(ns jameslintaylor.doctest.parse-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [are deftest is]]
   [jameslintaylor.doctest.parse :as parse]))

(deftest delimited-test
  (is (= ["foo" "bar" "baz"]
         (s/conform (parse/delimited #"\s*,\s*")
                    "foo, bar , baz")))
  (is (= parse/invalid
         (s/conform (parse/delimited #"," 2 2)
                    "foo, bar , baz"))
      "invalid when more splits than max-splits")
  (is (= parse/invalid
         (s/conform (parse/delimited #"," 4 4)
                    "foo, bar , baz"))
      "invalid when less splits than min-splits"))

(deftest assertion-spec-test
  (is (= parse/invalid
         (s/conform ::parse/assertion "not anything")))
  (is (= {:expr     "(foo bar)"
          :expected "baz"}
         (s/conform ::parse/assertion
                    "=> (foo bar)
                     baz")))
  (is (= {:expr     "(foo bar)"
          :expected "baz"
          :msg      "This message describes the assertion"}
         (s/conform ::parse/assertion
                    "This message describes the assertion
                     => (foo bar)
                     baz"))
      "messages are included in the assertion")
  (is (= {:expr     "(foo \"bar \\ \n  baz\")"
          :expected "\"foo\n\"bar\"\""}
         (s/conform ::parse/assertion
                    "=> (foo \"bar \\ \n  baz\")
                     \"foo\n\"bar\"\""))
      "literal quotes are captured verbatim"))

(deftest doctest-spec-test
  (is (= parse/invalid
         (s/conform ::parse/doctest "not anything")))
  (is (= parse/invalid
         (s/conform ::parse/doctest
                    "=> (is an)
                     assertion

                     not an assertion

                     => (also an)
                     assertion"))
      "cannot contain any invalid assertions")
  (is (= [{:expr     "(is an)"
           :expected "assertion"}
          {:expr     "(also an)"
           :expected "assertion"}]
         (s/conform ::parse/doctest
                    "=> (is an)
                     assertion

                     => (also an)
                     assertion"))
      "handles multiple assertions"))

(deftest doc-with-doctest-spec-test
  (is (= parse/invalid
         (s/conform ::parse/doc-with-doctest
                    "not-anything")))
  (is (= ["Docstring up until doctest"
          [{:expr     "(foo (bar baz))"
            :expected ":qux"}]]
         (s/conform ::parse/doc-with-doctest
                    "Docstring up until doctest

                     Usage:

                     => (foo (bar baz))
                     :qux"))))

(deftest doctest-assertions-test
  (is (nil? (parse/doctest-assertions "Just a regular old docstring.")))
  (let [doc "Just a regular old docstring with some usage examples.

             Usage:

             a ridiculous example
             => (is (foo :bar '(with quotes)))
             \"just crazy!\n\"really nuts!\"\""]
    (is (= [{:msg      "a ridiculous example"
             :expr     '(is (foo :bar '(with quotes)))
             :expected "just crazy!\\n\\\"really nuts!\\\""}]
           (parse/doctest-assertions doc))
        "special characters in literal strings are escaped")))
