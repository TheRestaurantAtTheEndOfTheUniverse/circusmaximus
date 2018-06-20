(ns circusmaximus.dictionary.util)

(def ^:const preprocessed-length 3)

(defn lookup-stem
  [stem]
  (subs stem 0 (min preprocessed-length (count stem))))


(def sorters
  {
   :speech-part [:adjective :adverb :conjunction :interjection :noun :number
                 :pronoun :verb :supine :verb-participle]

   :declension [:a :e :o :consonant :u :indeclinable :comparison :pronoun]

   :age [:archaic :early :classic :late :later :medieval :neo :modern :graffiti :unknown]

   :comparison [:positive :comparative :superlative :unknown]

   :word-frequency [:very-frequent :frequent :common :lesser :uncommon :very-rare :inscription :graffiti :pliny :unknown]
   :gender [:masculine :feminine :neuter :common :unknown]

   :tense [:present :imperfect :future :perfect :plusquamperfect :futureperfect :unknown]

   :nountype [:singularonly :pluralonly :abstractidea :propername :locale :person :thing :where :unknown]

   :declcase [:nominative :genetive :dative :accusative :ablative :vocative :locative :unknown]

   :mood [:indicative :subjunctive :imperative :infinitive :participle :supine :unknown]

   :grammaticalnumber [:singular :plural :unknown]

   :conjugation [:participle :a :e :consonant :i :esse :ire :defective :special]

   :numbertype [:cardinal :ordinal :distributive :adverb :unknown]

   :pronountype [:personal :relative :reflexive :demonstrative :interrogative :indefinitive :adjective :unknown]

   :verbtype [:unknown :esse :esse_compound :genetive :dative :ablative
              :transitive :intransitive :impersonal :deponent :semideponent :perfectdefinitive]

   :voice [:active :passive  :unknown]

   :wordcase [:nominative :genetive :dative :accusative :ablative :vocative :locative :unknown]

   :verbstem [:present :infinitive :perfect :participle]
   }
  )

(defn- index-extractor [prop]
  (fn [item]
    (if (sorters prop)
      (.indexOf (sorters prop) (item prop))
      prop)
      )
  )

(defn sort-parts [props items]
  (sort-by (apply juxt (map #(index-extractor %) props))
           items))
