(ns jameslintaylor.doctest.parse
  "Docstring parsing utilities."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as string]))

(def ^:const invalid
  ":clojure.spec.alpha/invalid

  Required because of a bug in the spec for clojure.core/let"
  ::s/invalid)

(defn delimited
  "Given a delimiting regex, return a spec for strings delimited by the
  regex.

  Optionally takes min-splits and max-splits to enforce a range of
  splits considered valid.

  Usage:

  => (s/conform (delimited #\"\\s*,\\s*\") \"foo, bar , baz\")
  [\"foo\" \"bar\" \"baz\"]

  invalid when more splits than max-splits
  => (s/conform (delimited #\";\" 1 2) \"a;b;c\")
  invalid

  invalid when less splits than min-splits
  => (s/conform (delimited #\";\" 3 4) \"a;b\")
  invalid"
  ([delimiting-regex]
   (s/and string?
          (s/conformer #(string/split % delimiting-regex))))
  ([delimiting-regex min-splits max-splits]
   (s/and string?
          (s/conformer #(let [d (string/split % delimiting-regex)
                              n (count d)]
                          (if (and (<= min-splits n)
                                   (>= max-splits n))
                            d
                            ::s/invalid))))))

(s/def ::assertion
  (let [msg-pattern  "(.+)?"
        line-pattern "((?:(?:\"[^\"]*\")|.)*)"
        assertion-re (re-pattern (str "\\s*"
                                      msg-pattern
                                      "\\s*=>\\s"
                                      line-pattern
                                      "\\n\\s*"
                                      line-pattern
                                      "\\s*"))]
    (s/conformer #(if-let [[_ msg expr expected]
                           (re-matches assertion-re %)]
                    (cond-> {:expr expr :expected expected}
                      msg (assoc :msg msg))
                    invalid))))

(s/def ::doctest
  (s/and (delimited #"\n\n")
         (s/coll-of ::assertion)))

(s/def ::doc-with-doctest
  (s/and (delimited #"\n\n.*Usage:\n\n" 2 2)
         (s/tuple string? ::doctest)))

(defn- resolve-sym
  [ns sym]
  (if-let [{:keys [name ns]} (meta (ns-resolve ns sym))]
    (symbol (str ns) (str name))
    sym))

(defn- resolve-all
  [ns expr]
  (if (seq? expr)
    `(~@(map (partial resolve-all ns) expr))
    (if (symbol? expr)
      (resolve-sym ns expr)
      expr)))

(defn- escape
  [s]
  (string/escape s {\"       "\\\\\\\""
                    \newline "\\\\n"}))

(defn- escape-literal
  [s]
  (if (= \" (first s) (last s))
    (let [cs (subs s 1 (dec (count s)))]
      (format "\"%s\"" (escape cs)))
    s))

(defn doctest-assertions
  "Given a var, return its doctest assertions or nil if the var does not
  contain a doctest.

  A doctest assertion will be a map including values for:
  - :expr
  - :expected

  A doctest assertion may include values for:
  - :msg

  Usage:

  returns nil when no doctest
  => (doctest-assertions (defn foo \"No examples!\" []))
  nil"
  [var]
  (let [{:keys [doc ns]} (meta var)
        doc-with-doctest (s/conform ::doc-with-doctest doc)]
    (when-not (s/invalid? doc-with-doctest)
      (let [[_ assertions] doc-with-doctest
            read-with-syms (comp (partial resolve-all (:ns (meta var)))
                                 read-string
                                 escape-literal)]
        (map (fn [a]
               (-> a
                   (update :expr read-with-syms)
                   (update :expected read-with-syms)))
             assertions)))))
