(ns circusmaximus.dictionary.analyser
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log :refer [info error]]
            [circusmaximus.dictionary.dictionary :refer [dictionary]]
            [circusmaximus.dictionary.util :refer :all]
            [clojure.core.match :refer [match]]
            ))

(def ^:const use-locative true)

(defn nounword? [s]
  (contains? #{:adjective :adverb :noun :number :pronoun} (:speech-part s)))

(defmulti fits-stem? #(:speech-part %2))

(defmethod fits-stem? :adjective [stem word]
  (or (= (:nominative word) stem)
      (= (:genetive word) stem)))

(defmethod fits-stem? :adverb[stem word]
  (or (= (:positive word) stem)
      (= (:comparative word) stem)
      (= (:superlative word) stem)))

(defmethod fits-stem? :conjunction [stem word]
  (or (= (:form word) stem)))

(defmethod fits-stem? :interjection [stem word]
  (or (= (:form word) stem)))

(defmethod fits-stem? :noun [stem word]
  (or (= (:nominative word) stem)
      (= (:genetive word) stem)))

(defmethod fits-stem? :number [stem word]
  (or (= (:cardinal word) stem)
      (= (:ordinal word) stem)
      (= (:distributive word) stem)
      (= (:adverb word) stem)))

(defmethod fits-stem? :preposition [stem word]
  (or (= (:form word) stem)))

(defmethod fits-stem? :pronoun [stem word]
  (or (= (:nominative word) stem)
      (= (:genetive word) stem)))

(defmethod fits-stem? :verb [stem word]
(or (= (:present word) stem)
    (= (:infinitive word) stem)
    (= (:perfect word) stem)
    (= (:participle word) stem)))

(defmethod fits-stem? :default [stem word]
  false)

(defn fits-nounword-declension? [word ending]
  (or (= (:declension ending) :comparison)
      (and (= (:declension ending) (:declension word))
           (or (= 0 (:variant ending))
               (= (:variant ending) (:variant word))))))

(def compatible-with-common #{:unknown :feminine :masculine :common})

(defn fits-nounword-gender? [word ending]
  (let [eg (:gender ending)
        wg (:gender word)]
  (and (or (not= eg :common)
           (contains? compatible-with-common wg))
       (or (= wg :unknown)
           (= eg :unknown)
           (= eg :common)
           (= eg wg)))))

(defn fits-nounword-number? [word ending]
  (let [wn (:number word)
        en (:number ending)
        ]
    (and (not (and (= wn :singular-only)
                   (= en :plural-only)))
         (not (and (= en :singular-only)
                   (= wn :plural-only))))))

(defn fits-nounword? [word ending]
  (and (fits-nounword-number? word ending)
       (fits-nounword-declension? word ending)
       (fits-nounword-gender? word ending)
       ))

(defn- fits-nounword-stem? [stem word ending]
  (= (if (and (= (:number ending :singular))
              (or (= (:wordcase ending) :nominative)
                  (= (:wordcase ending) :vocative)
                  (and (= (:wordcase ending) :accusative)
                       (= (:gender word) :neuter)))
             )
       (:nominative word)
       (:genetive word))
     stem))

(defn fits-noundword-ending [stem word ending]
  (and (nounword? word)
       (nounword? ending)
       (fits-nounword? word ending)
       (fits-nounword-stem? stem word ending)))

(defmulti fits-ending? #(:speech-part %3))

(defmethod fits-ending? :adjective [stem word ending]
  (and (= (:speech-part word) :adjective)
       (fits-nounword-declension? word ending)
       (= (case (:comparison ending)
            :unknown nil
            :positive (if (and (= (:word-case ending) :nominative)
                               (= (:number ending) :singular))
                        (:nominative word)
                        (:genetive word))
            :comparative (:comparative word)
            :superlative (:superlative word))
          stem)))

(defmethod fits-ending? :adverb [stem word ending]
  (and (= (:speech-part word) :adverb)
       (if (= (:comparison ending) :unknown)
         (or (= stem (:positive word))
             (= stem (:comparative word))
             (= stem (:superlative word)))
         (= stem (get word (:comparison ending))))
       ))
(defmethod fits-ending? :conjunction [stem word ending]
  (= (:speech-part word) :conjunction))

(defmethod fits-ending? :interjection [stem word ending]
  (= (:speech-part word) :interjection))

(defmethod fits-ending? :noun [stem word ending]
  (and (or use-locative
           (not= (:word-case ending) :locative))
       (= (:speech-part word) :noun)
       (fits-noundword-ending stem word ending)))

(defmethod fits-ending? :number [stem word ending]
  (and (= (:speech-part word) :number)
       (= stem (get word (:type ending)))
       (fits-nounword-declension? word ending)

       (or (not= (:type ending) :cardinal)
           (and (= (:number ending) :plural)
                (> (:numbervalue word) 1))
           (and (= (:number ending) :singular)
                (<= (:numbervalue word) 1))
           (= (:number ending) :unknown)
           )))

(defmethod fits-ending? :preposition [stem word ending]
  (and (= (:speech-part word) :preposition)
       (= (:word-case word) (:word-case ending))))

(defmethod fits-ending? :pronoun [stem word ending]
  (and (= (:speech-part word) :pronoun)
       (fits-nounword-number? word ending)
       (fits-nounword-declension? word ending)))

(defmethod fits-ending? :verb [stem word ending]
  (and (= (:speech-part word) :verb)
       (or (= (:conjugation ending) :participle)
           (= (:conjugation ending) (:conjugation word)))
       (or (= (:variant ending) 0)
           (= (:variant ending) (:variant word)))
       (= stem (get word (:verbstem ending)))
       ))

(defmethod fits-ending? :supine [stem word ending]
  (and (= (:speech-part word) :verb)
       (or (= (:conjugation ending) :participle)
           (= (:conjugation ending) (:conjugation word)))
       (or (= (:variant ending) 0)
           (= (:variant ending) (:variant word)))
       (= stem (:participle word))
       ))

(defmethod fits-ending? :verb-participle [stem word ending]
  (and (= (:speech-part word) :verb)
       (or (= (:conjugation ending) :participle)
           (= (:conjugation ending) (:conjugation word)))
       (or (= (:variant ending) 0)
           (= (:variant ending) (:variant word)))
       (= stem (get word (:verbstem ending)))
       ))

(defmethod fits-ending? :default [stem word ending]
  (error "Unhandled ending" ending)
  false)

(defn properties= [v1 v2 properties]
  (loop [p properties]
    (if (empty? p)
      true
      (if (= (get v1 (first p))
             (get v2 (first p)))
        (recur (rest p))
        false
        ))))

(defmulti ending-superseded-by? (fn [e1 e2]
                                  (:speech-part e1)))

(defn- supersedes-gender?
  [e1 e2]
  (or (= (:gender e1) (:gender e2))
      (and (= (:gender e1) :unknown)
           (not= (:gender e2) :unknown)
           )))

(defn- supersedes-comparison?
  [e1 e2]
  (or (= (:comparison e1) (:comparison e2))
      (and (= (:comparison e1) :unknown)
           (not= (:comparison e2) :unknown)
           )))


(defmethod ending-superseded-by? :noun [ne1 ne2]
  (and (properties= ne1 ne2 [:ending :declension :number :wordcase])
       (supersedes-gender? ne1 ne2)))

(defmethod ending-superseded-by? :adverb [ae1 ae2]
  (and (properties= ae1 ae2 [:ending])
       (supersedes-comparison? ae1 ae2)))


(defmethod ending-superseded-by? :default [e1 e2]
  false)

(defn- prune-endings [endings]
  (let [s-fn (fn [e]
               (map #(not (ending-superseded-by? e %))
                    (remove #{e} endings)))
        super (map s-fn endings)]
    (filter #(every? identity (s-fn %))
            endings)))

(defn analyse-single [word]
  (let [fitting-endings (filter #(str/ends-with? word (:ending %))
                                (:endings @dictionary))
        word-len        (count word)
        analysed-words  (reduce (fn [results e]
                                  (let [stem          (subs word 0 (- word-len (count (:ending e))))
                                        fitting-words (filter (fn [dictword]
                                                                (and
                                                                 (fits-stem? stem dictword)
                                                                 (fits-ending? stem dictword e)))
                                                              (concat (get-in @dictionary [:stem-lookup (lookup-stem stem)])
                                                                      (:unprocessed-stems @dictionary)))
                                        ]
                                    (reduce (fn [results dictword]
                                              (if (contains? results (:id dictword))
                                                (update-in results [(:id dictword) :endings] conj e)
                                                (assoc results (:id dictword) (assoc dictword :endings [e]))))
                                            results
                                            fitting-words)))
                                {}
                                fitting-endings)]
    (apply concat
     ((juxt filter remove)
      #(= (:conjugation %) :esse)
      (map #(assoc %
                   :endings (prune-endings (:endings %)))
           (vals analysed-words))
     ))))

(defn- keep-vpars [esse v]
  (assoc v :endings
         (map #(assoc % :esse-ending esse)
              (filter #(= (:speech-part %)
                          :verb-participle)
                      (:endings v)))))

(defn- participle? [v]
  (and (= (:speech-part v) :verb)
       (some #(= (:speech-part %) :verb-participle) (:endings v))))

(defn- participle-esse? [esse]
  (and (= (:speech-part esse) :verb)
       (= (:conjugation esse) :esse)
       (contains? #{:present :imperfect :future}
                  (-> esse :endings first :tense))))

(defn- combine-participle [verb esse]
  (assoc verb
         :esse (dissoc esse :endings)
         :endings
         (map #(assoc % :esse-ending (first (:endings esse)))
              (:endings verb)
              )))

(defn- combine-participles [analysed-words]
  (loop [[verb esse & tail-end :as words] analysed-words
         acc []]
    (if (nil? esse) (conj acc verb)
        (if (and (participle? verb) (participle-esse? esse))
          (recur tail-end (conj acc (combine-participle verb esse)))
          (recur (rest words) (conj acc verb))))))

(defn analyse [word]
  (let [words (str/split word #" ")]
    (combine-participles (mapcat analyse-single words))))
