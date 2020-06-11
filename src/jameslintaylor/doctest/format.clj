(ns jameslintaylor.doctest.format
  "Docstring formatting utilities."
  (:require
   [cljfmt.main :as cljfmt]
   [clojure.string :as string]
   [clojure.test :as test]
   [jameslintaylor.doctest.parse :as parse]))

(def ^:private format-cljfmt
  (comp (partial #'cljfmt/reformat-string {}) format))

(defn wrap
  [n form]
  (with-meta form {::wrap n}))

(defmulti form-str type)

(defn- seq-form-str [seq-form]
  (let [nwrap (::wrap (meta seq-form) (count seq-form))
        [h t] (split-at nwrap seq-form)]
    (format-cljfmt "(%s%s)"
                   (string/join " " (map form-str h))
                   (apply str (map (comp (partial str "\n") form-str) t)))))

(defmethod form-str clojure.lang.Cons
  [form]
  (seq-form-str form))

(defmethod form-str clojure.lang.IPersistentList
  [form]
  (seq-form-str form))

(defmethod form-str java.lang.String
  [form]
  (format "\"%s\"" form))

(defmethod form-str java.util.regex.Pattern
  [form]
  (format "#\"%s\"" form))

(defmethod form-str nil
  [_]
  "nil")

(defmethod form-str :default
  [form]
  (str form))

(defn assertion-form
  [assertion]
  (let [{:keys [expected expr msg]} assertion]
    (if msg
      (wrap 2 `(~'is (~'= ~expected ~expr) ~msg))
      `(~'is (~'= ~expected ~expr)))))

(defn var-test-form
  [var]
  (when-let [assertions (parse/doctest-assertions var)]
    (wrap 3 `(~'deftest ~(symbol "^:doctest") ~(symbol (str (-> var meta :name) "-test"))
              ~@(map assertion-form assertions)))))

(defn ns-form
  [ns]
  (wrap 2 `(~'ns ~(symbol (str ns "-doctest"))
            "Generated by doctest."
            ~(wrap 1 `(:require
                       ~(symbol (str ns))
                       [~'clojure.test :refer [~'deftest ~'is]])))))

(defn format-test-file
  [ns]
  (let [var-tests (into []
                        (comp (map var-test-form)
                              (filter identity)
                              (map form-str))
                        (vals (ns-interns ns)))]
    (format "%s%s"
            (form-str (ns-form ns))
            (apply str (map (partial str "\n\n") var-tests)))))
