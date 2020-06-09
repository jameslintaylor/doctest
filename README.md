Python-like doctest support for Clojure.

# Usage

Scan your `./src` directory and generate doctests:

```
$ clj -m jameslintaylor.doctest
Generated tests in ./target/doctest/
```

# Test format

For each var on your source path containing `Usage:` doc examples, e.g.:

```clojure
(ns jameslintaylor.example)

(defn foo
  "I don't do a whole lot.

  Usage:

  => (foo 1)
  :foo

  foo always returns :foo
  => (foo nil)
  :foo

  => (= (foo 1) (foo 2))
  true"
  [_]
  :foo)
```

Doctest will generate a corresponding test, e.g.:

```clojure
(ns jameslintaylor.example-test
  "Generated by doctest."
  (:require
   clojure.test
   jameslintaylor.example))

(clojure.test/deftest ^:doctest foo-test
  (clojure.test/is (= :foo (jameslintaylor.example/foo 1)))
  (clojure.test/is (= :foo (jameslintaylor.example/foo nil))
                   "foo always returns :foo")
  (clojure.test/is (= true (clojure.core/= (jameslintaylor.example/foo 1) (jameslintaylor.example/foo 2)))))
```

# Running the generated tests

Add `./target/doctest` as an extra path to your test runner of choice and run your tests as you would otherwise.
