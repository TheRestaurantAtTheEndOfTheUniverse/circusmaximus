(ns circusmaximus.wordhelpers)

(defn pronoun-stem [{:keys [nominative genetive] :as word} {:keys [number wordcase]}]
  (if (and (or (= wordcase :genetive)
               (= wordcase :dative))
           (= number :singular))
    genetive
    nominative))
