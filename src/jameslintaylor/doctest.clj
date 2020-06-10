(ns jameslintaylor.doctest
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.namespace.find :as namespace.find]
   [jameslintaylor.doctest.format :as format]))

(defn ns-test-path
  [ns]
  (-> ns
      (string/replace "." "/")
      (string/replace "-" "_")))

(defn write-ns-tests
  [ns]
  (let [f (str "./target/doctest/" (ns-test-path ns) "_test.clj")]
    (io/make-parents f)
    (spit f (format/format-test-file ns))))

(defn dir-nses
  [dir]
  (namespace.find/find-namespaces-in-dir (clojure.java.io/file dir)))

(defn gen-doctests []
  (doseq [ns (dir-nses "./src")]
    (write-ns-tests ns)))

(defn -main [& [mode]]
  (gen-doctests)
  (println "Generated tests in" "./target/doctest/"))

;;;
;;; Test vars
;;;

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

(defn bar
  "Best function ever!

  Usage:

  => (bar 1 2)
  3"
  [x y]
  (+ x y))
