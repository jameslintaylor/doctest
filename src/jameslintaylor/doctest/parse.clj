(ns jameslintaylor.doctest.parse
  "Docstring parsing utilities."
  (:require
   [clojure.edn :as edn]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]))

(defn delimited
  "Given a delimiting regex, return a spec for strings delimited by the
  regex.

  Optionally takes min-splits and max-splits to enforce a range of
  splits considered valid."
  ([delimiting-regex]
   (s/and string?
          (s/conformer #(string/split % delimiting-regex))))
  ([delimiting-regex min-splits max-splits]
   (s/and string?
          (s/conformer #(let [d (string/split % delimiting-regex max-splits)
                              n (count d)]
                          (if (and (<= min-splits n)
                                   (>= max-splits n))
                            d
                            ::s/invalid))))))

(def ^:const invalid
  ":clojure.spec.alpha/invalid

  Required because of a bug in the spec for clojure.core/if-let"
  ::s/invalid)

(s/def ::assertion
  (s/conformer #(if-let [[_ msg expr expected]
                         (re-matches #"\s*(\S.*)?\s*=>\s*(\(.*\))\s*(.*)" %)]
                  {:msg msg :expr expr :expected expected}
                  invalid)))

(s/def ::doctest
  (s/and (delimited #"\n\n")
         (s/conformer #(or (rest %) ::s/invalid))
         (s/coll-of ::assertion)))

(s/def ::docstring-with-doctest
  (s/and (delimited #"Usage:" 2 2)
         (s/tuple string? ::doctest)))

(def ^:private doc (comp :doc meta))

(defn has-doctest?
  "Given a var, return true iff the var's doc contains a doctest."
  [var]
  (s/valid? ::docstring-with-doctest (doc var)))

(defn- resolve-sym
  [ns sym]
  (let [{:keys [name ns]} (meta (ns-resolve ns sym))]
    (symbol (str ns) (str name))))

(defn- resolve-all
  [ns expr]
  (if (seq? expr)
    `(~@(map (partial resolve-all ns) expr))
    (if (symbol? expr)
      (resolve-sym ns expr)
      expr)))

(defn doctest-assertions
  [var]
  (let [[_ assertions] (s/conform ::docstring-with-doctest (doc var))
        read-with-syms (comp (partial resolve-all (:ns (meta var)))
                             edn/read-string)]
    (map (fn [a]
           (-> a
               (update :expr read-with-syms)
               (update :expected read-with-syms)))
         assertions)))

