(ns circusmaximus.dictionary.util)

(def ^:const preprocessed-length 3)

(defn lookup-stem
  [stem]
  (subs stem 0 (min preprocessed-length (count stem))))
