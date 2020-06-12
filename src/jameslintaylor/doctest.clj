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
  (let [f (str "./target/doctest/" (ns-test-path ns) "_doctest.clj")]
    (io/make-parents f)
    (spit f (format/test-file-str ns))))

(defn dir-nses
  [dir]
  (namespace.find/find-namespaces-in-dir (clojure.java.io/file dir)))

(defn gen-doctests []
  (doseq [ns (dir-nses "./src")]
    (write-ns-tests ns)))

(defn -main [& [mode]]
  (gen-doctests)
  (println "Generated tests in" "./target/doctest/"))
