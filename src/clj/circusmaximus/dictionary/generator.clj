(ns circusmaximus.dictionary.generator
  (:require [circusmaximus.dictionary.analyser :as ana]
            [circusmaximus.dictionary.util :as util]
            [circusmaximus.dictionary.dictionary :as dict]
            [circusmaximus.wordhelpers :refer :all]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))


(def ^:const allowed-verb-combination
  #{[:indicative :present :active]
    [:subjunctive :present :active]
    [:indicative :imperfect :active]
    [:subjunctive :imperfect :active]
    [:indicative :future :active]
    [:imperative :present :active]
    [:imperative :future :active]
    [:infinitive :present :active]
    [:participle :present :active]

    [:indicative :present :passive]
    [:subjunctive :present :passive]
    [:indicative :imperfect :passive]
    [:indicative :future :passive]
    [:imperative :present :passive]
    [:imperative :future :passive]
    [:infinitive :present :passive]

    [:indicative :perfect :active]
    [:subjunctive :perfect :active]
    [:indicative :plusquamperfect :active]
    [:subjunctive :plusquamperfect :active]
    [:indicative :futureperfect :active]
    [:infinitive :perfect :active]

    [:indicative :perfect :passive]
    [:subjunctive :perfect :passive]
    [:indicative :plusquamperfect :passive]
    [:subjunctive :plusquamperfect :passive]
    [:indicative :futureperfect :passive]
    [:participle :perfect :passive]
    [:infinitive :perfect :passive]
    [:infinitive :future :active]
    [:participle :future :active]})

(defn- gender-compatible? [word-gender end-gender]
  (or (= word-gender end-gender)
      (= end-gender :unknown)
      (and (= end-gender :common)
           (or (= word-gender :masculine)
               (= word-gender :feminine)))
      ))

(defn- adjective-stem [word wordcase comparison]
  (case comparison
    :positive (if (= wordcase :nominative) (:nominative word) (:genetive word))
    :comparative (:comparative word)
    :superlative (:superlative word)))

(defn generate-adjective [word number wordcase gender comparison endings]
  (let [stem (adjective-stem word wordcase comparison)
        matching (util/sort-parts [:frequency]
                                  (filter (fn [ending]
                                            (and (= (:speech-part ending) :adjective)
                                                 (ana/fits-ending? stem word ending)
                                                 (= (:number ending) number)
                                                 (= (:wordcase ending) wordcase)
                                                 (gender-compatible? gender (:gender ending))
                                                 (= (:comparison ending) comparison)
                                                 ))
                                          endings))]
    (if (seq matching)
      (map #(str stem (:ending %)) matching))))

(defn generate-adverb [word comparison endings]
  (let [stem (get word comparison)
        matching (if-not (str/blank? stem)
                   (util/sort-parts [:frequency]
                                    (filter (fn [ending]
                                              (and (= (:speech-part ending) :adverb)
                                                   (ana/fits-ending? stem word ending)
                                                   (= (:comparison ending) comparison)))
                                            endings)))]
    (if (seq matching)
      (map #(str stem (:ending %)) matching))))

(defn generate-noun [word number wordcase endings]
  (let [stem (if (and (= wordcase :nominative)
                      (= number :singular))
               (:nominative word)
               (:genetive word))
        matching (if-not (str/blank? stem)
                   (util/sort-parts [:frequency]
                                    (filter (fn [ending]
                                              (and (= (:speech-part ending) :noun)
                                                   (ana/fits-ending? stem word ending)
                                                   (= (:number ending) number)
                                                   (= (:wordcase ending) wordcase)))
                                            endings)))]
    (if (seq matching)
      (map #(str stem (:ending %)) matching))))

(defn generate-pronoun [word number wordcase gender endings]
  (let [matching (util/sort-parts [:frequency]
                                  (filter (fn [ending]
                                            (let [stem (pronoun-stem word ending)]
                                              (and (= (:speech-part ending) :pronoun)
                                                   (ana/fits-ending? stem word ending)
                                                   (= (:number ending) number)
                                                   (or (= (:gender ending) gender)
                                                       (= (:gender ending) :unknown))
                                                   (= (:wordcase ending) wordcase))))
                                            endings))]
    (if (seq matching)
      (map #(str (pronoun-stem word %) (:ending %)) matching))))

(defn generate-infinitive [word voice tense endings]
  (if-not (contains? allowed-verb-combination [:infinitive tense voice])
    (throw (ex-info "Parameters not allowed" {:voice voice
                                              :tense tense}))
    (util/sort-parts [:frequency]
                     (filter (fn [ending]
                               (and (= (:speech-part ending) :verb)
                                    (ana/fits-ending? (word (:verbstem ending)) word ending)
                                    (= (:voice ending) voice)
                                    (= (:mood ending) :infinitive)
                                    (= (:tense ending) tense)))
                             endings))))

(defn generate-supine [word endings]
  (util/sort-parts [:frequency]
                   (filter (fn [ending]
                             (and (= (:speech-part ending) :supine)
                                  (ana/fits-ending? (:participle word) word ending)))
                           endings)))

(defn generate-verbform [word number voice mood tense person endings]
  (util/sort-parts [:frequency]
                   (filter (fn [ending]
                             (and (= (:speech-part ending) :verb)
                                  (ana/fits-ending? (word (:verbstem ending)) word ending)
                                  (= (:number ending) number)
                                  (= (:voice ending) voice)
                                  (= (:mood ending) mood)
                                  (= (:tense ending) tense)
                                  (= (:person ending) person)))
                           endings)))

(defn generate-participle [word number tense wordcase gender endings]
  (util/sort-parts [:frequency]
                   (filter (fn [ending]
                             (and (= (:speech-part ending) :verb-participle)
                                  (ana/fits-ending? (word (:verbstem ending)) word ending)
                                  (= (:number ending) number)
                                  (= (:tense ending) tense)
                                  (= (:wordcase ending) wordcase)
                                  (= (:gender ending) gender)))
                           endings)))


(defn generate-verb [word voice mood tense wordcase number gender person endings]
  (cond
    (= mood :infinitive) (generate-infinitive word voice tense endings)
    (= mood :supine) (let [result (generate-supine word endings)]
                       (if (seq result)
                         (map #(str (:participle word) (:ending %)) result)))

    (and (= voice :passive)
         (contains? #{:perfect :plusquamperfect :futureperfect} tense))
    (let [word-result (generate-participle word number :perfect wordcase gender endings)
          esse-tense (case tense
                       :perfect :present
                       :plusquamperfect :imperfect
                       :futureperfect :future
                       nil)
          esse-form (if (seq word-result)
                      (first (generate-verb (:esse @dict/dictionary) :active mood esse-tense wordcase number gender person endings)))
          ]
      (if (and (seq word-result)
               esse-form)
        (map #(str (:participle word) (:ending %) " " (get (:esse @dict/dictionary)
                                                           (:verbstem esse-form))
                   (:ending esse-form)) word-result)))

    (and (= voice :active)
         (= tense :future)
         (= mood :participle))
    (let [result (generate-participle word number tense wordcase gender endings)]
      (if (seq result)
        (map #(str (:participle word) (:ending %)) result)))

    :else (generate-verbform word number voice mood tense person endings)
    )
  )

(defn noun-declension-table
  ([word endings]
  (reduce (fn [table [number wordcase]]
            (assoc-in table [number wordcase]
                      (generate-noun word number wordcase endings)))
          {}
          (combo/cartesian-product [:singular :plural]
                                   [:nominative :genetive :accusative :dative :ablative]))
  )
  ([word]
   (noun-declension-table word (:endings @dict/dictionary))))

(defn pronoun-declension-table
  ([word endings]
  (reduce (fn [table [number wordcase gender]]
            (assoc-in table [number wordcase gender]
                      (generate-pronoun word number wordcase gender endings)))
          {}
          (combo/cartesian-product [:singular :plural]
                                   [:nominative :genetive :accusative :dative :ablative]
                                   [:masculine :feminine :neuter])))
  ([word]
   (pronoun-declension-table word (:endings @dict/dictionary))))


(defn adjective-declension-table
  ([word comparison endings]
  (reduce (fn [table [number gender wordcase]]
            (assoc-in table [number wordcase gender]
                      (generate-adjective word number wordcase gender comparison endings)))
          {}
          (combo/cartesian-product [:singular :plural]
                                   [:nominative :genetive :accusative :dative :ablative]
                                   [:masculine :feminine :neuter]
                                   )))
  ([word comparison]
   (adjective-declension-table word comparison (:endings @dict/dictionary))))

(comment

  (let [word (second (:adjectives @dict/dictionary))]
    (adjective-declension-table word :positive))

  (let [word (second (:adverbs @dict/dictionary))]
     (generate-adverb word
                      :comparative
                      (:endings @dict/dictionary))
     )

  (let [word (first (:pronouns @dict/dictionary))]
     (pronoun-declension-table word))


  (let [word (second (:nouns @dict/dictionary))]
    (noun-declension-table word))

  (let [word (first (:verbs @dict/dictionary))]
    (clojure.pprint/pprint word)
    (clojure.pprint/pprint
     (generate-infinitive word :active :perfect (:endings @dict/dictionary))))

  (let [word (first (:verbs @dict/dictionary))]
    (clojure.pprint/pprint word)
    (clojure.pprint/pprint
     (generate-supine word (:endings @dict/dictionary))))

  (let [word (first (:verbs @dict/dictionary))]
     (generate-verb word :passive :indicative :perfect :nominative :singular :masculine 1 (:endings @dict/dictionary)))

  (let [word (first (:verbs @dict/dictionary))]
    (clojure.pprint/pprint word)
    (clojure.pprint/pprint
     (generate-participle word :plural :perfect :genetive :masculine (:endings @dict/dictionary))))


  )
