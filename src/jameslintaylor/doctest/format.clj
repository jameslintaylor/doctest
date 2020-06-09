(ns jameslintaylor.doctest.format
  "Docstring formatting utilities."
  (:require
   [cljfmt.main :as cljfmt]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :as test]
   [jameslintaylor.doctest.parse :as parse]))

(defn- format-assertion
  [assertion]
  (let [{:keys [expected expr msg]} assertion
        args                        (if msg
                                      ["(~s (~s ~s ~s)\n ~s)" `test/is '= expected expr msg]
                                      ["(~s (~s ~s ~s))" `test/is '= expected expr])]
    ;; Use cl-format to handle nil
    (apply pprint/cl-format nil args)))

(defn- format-var-test
  [var]
  (when-let [assertions (parse/doctest-assertions var)]
    (format "(%s ^:doctest %s%s)"
            `test/deftest
            (str (-> var meta :name) "-test")
            (apply str (map (comp (partial str "\n") format-assertion) assertions)))))

(defn format-ns-decl
  [ns]
  (let [requires (format "(:require\nclojure.test\n%s)" ns)]
    (format "(ns %s-test\n\"Generated by doctest.\"\n%s)"
            ns
            requires)))

(def ^:private format-cljfmt
  (partial #'cljfmt/reformat-string {}))

(defn format-test-file
  [ns]
  (let [var-tests (->> (vals (ns-interns ns))
                       (filter parse/has-doctest?)
                       (map format-var-test-decl))]
    (format-cljfmt
     (format "%s%s"
             (format-ns-decl ns)
             (apply str (map (partial str "\n\n") var-tests))))))