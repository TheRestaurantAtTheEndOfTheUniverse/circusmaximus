(ns circusmaximus.dictionary.dictionary
  (:require [clojure.java.jdbc :as j]
            [clojure.tools.logging :as log :refer [info error]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clj-time.coerce :as tc]
            [cheshire.core :as json]
            [hugsql.core :as hugsql]
            [clojure.string :as str]
            [circusmaximus.dictionary.dictschema :as ds]
            [circusmaximus.dictionary.util :refer :all]
            [schema.core :as s]
            [clojure.core.match :refer [match]]
            [environ.core :refer [env]]
            ))



(def ^:const simple-speech-part #{:conjunction :interjection :preposition})

(defonce conn (atom nil))


(def dict-enums
  #{:comparison :age :number :declension :frequency :wordcase :gender
   :conjugation
   :tense :voice :verbstem
   :mood :type :nountype
   })


(defn create-db-spec
  []
  {:subprotocol "postgresql"
   :subname     (env :cm-db)
   :user        (env :cm-user)
   :password    (env :cm-password)})

(defn init-db []
  (let [dbspec (create-db-spec)
        jdbc-con (j/get-connection dbspec)
        ]
    (reset! conn {:connection jdbc-con})))

(.addShutdownHook
  (Runtime/getRuntime)
  (Thread.
   #(if (and @conn
             (not (.isClosed (:connection @conn))))
       (do (info "Closing DB connection")
           (.close (:connection @conn))))))


(extend-protocol j/IResultSetReadColumn
  org.postgresql.util.PGobject
  (result-set-read-column [x _ _]
    (if-let [val (.getValue x)]
      (case (.getType x)
        "jsonb" (json/parse-string val)
        "json" (json/parse-string val)
        "date" (tc/from-sql-date val)
        "timestamp" (tc/from-sql-date val)
        (.getValue x)
        )
      (.getValue x))))


(hugsql/def-db-fns (io/resource "circusmaximus/dictionary.sql"))

(defn- keywordize-row [key-cols row]
  (reduce-kv #(if (contains? key-cols %2)
                (assoc %1 %2 (keyword %3))
                %1)
             row
             row))

(def keywordize-enums (partial keywordize-row dict-enums))


(defn base-form-dispatch [word]
  (if (contains? simple-speech-part (:speech-part word))
    :simple
    (:speech-part word)
  ))

(defmulti base-forms base-form-dispatch)

(defmethod base-forms :noun [noun]
  (let [with-endings (fn [ne ge]
                       [(str (:nominative noun) ne) (str (:genetive noun) ge)])]
    (match [(:declension noun) (:variant noun)]
           [:a 1] (with-endings "a" "ae")
           [:a 6] (with-endings "e" "es")
           [:a 7] (with-endings "es" "ae")
           [:a 8] (with-endings "as" "ae")
           [:o (:or 1 9)] (with-endings "us" "i")
           [:o 2] (with-endings "um" "i")
           [:o 3] (with-endings "" "i")
           [:o 4] (with-endings (if (= (:gender noun) :masculine) "us" "um") "(i)")
           [:o 5] (with-endings "us" "")
           [:o (:or 6 7)] (with-endings "os" "i")
           [:o 8] (with-endings "on" "i")
           [:consonant (:or 7 9)] (with-endings "" "os/is")
           [:consonant _] (with-endings "" "is")
           [:u 1] (with-endings "us" "us")
           [:u 2] (with-endings "u" "us")
           [:u 3] (with-endings "us" "u")
           [:e _] (with-endings "es" "ei")
           [_ _] (with-endings "Not implemented" "Not implemented")
           )
    ))

(defmethod base-forms :adjective [adj]
  (let [with-endings (fn ([m f n]
                       [(str (:nominative adj) m)
                        (str (:genetive adj) f)
                        (str (:genetive adj) n)
                        ])
                       ([m f]
                       [(str (:nominative adj) m)
                        (str (:genetive adj) f)]))
        unknown (fn [m g c s]
                       [(str (:nominative adj) m)
                        (str (:genetive adj) g)
                        (str (:comparative adj) c)
                        (str (:superlative adj) s)])
                       ]
    (match [(:comparison adj) (:declension adj) (:variant adj)]
           [:positive :a (:or 1 4)] (with-endings "us" "a" "um")
           [:positive :a 2] (with-endings "" "a" "um")
           [:positive :a 3] (with-endings "us" "a" "um (gen -ius)")
           [:positive :a 5] (with-endings "us" "a" "ud")
           [:positive :a 6] (with-endings "" "e" "-")
           [:positive :a 7] (with-endings "es" "es" "es")
           [:positive :o 3] (with-endings "es" "es" "es")
           [:positive :o 6] (with-endings "os" "os" "-")
           [:positive :o 7] (with-endings "os" "-" "-")
           [:positive :o 8] (with-endings "-" "-" "on")
           [:positive :consonant 1] (with-endings "" "is")
           [:positive :consonant 2] (with-endings "is" "is" "e")
           [:positive :consonant 3] (with-endings "" "is" "e")
           [:positive :consonant _] nil
           [:positive :indeclinable _] [(str (:nominative adj) " undeclined")]
           [:comparative _ _] [(str (:nominative adj) "or")
                               (str (:nominative adj) "or")
                               (str (:nominative adj) "us")]
           [:superlative _ _] [(str (:nominative adj) "mus")
                               (str (:nominative adj) "ma")
                               (str (:nominative adj) "mum")]
           [:unknown :a 1] (unknown "us" "a -um" "or -or -us" "mus -a -um")
           [:unknown :a 2] (unknown "" "a -um" "or -or -us" "mus -a -um")
           [:unknown :consonant 1] (unknown "" "is (gen.)" "or -or -us" "mus -a -um")
           [:unknown :consonant 2] (unknown "" "e" "or -or -us" "mus -a -um")
           [:unknown :consonant 3] (unknown "" "is -e" "or -or -us" "mus -a -um")
           [:unknown :indeclinable _] [(str (:nominative adj) " undeclined")
                                       (str (:comparative adj) "or -or -us")
                                       (str (:superlative adj) "mus -a -um")])))

(defmethod base-forms :adverb [adv]
  (vec ((juxt :positive :comparative :superlative) adv)))

(defmethod base-forms :simple [s]
  [(:form s)])

(defmethod base-forms :number [n]
  (let [with-endings (fn ([c o d a]
                          [(str (:cardinal n) c)
                           (str (:ordinal n) o)
                           (str (:distributive n) d)
                           (str (:adverb n) a)])
                       ([f m ne]
                        [(str (:cardinal n) f)
                         (str (:cardinal n) m)
                         (str (:cardinal n) ne)]))]
    (match [(:type n) (:declension n) (:variant n)]
           [:unknown :a 1] (with-endings "us -a -um" "us -a -um" "i -ae -a" "")
           [:unknown :a 2] (with-endings "o -ae -o" "us -a -um" "i -ae -a" "")
           [:unknown :a 3] (with-endings "es -es -ia" "us -a -um" "i -ae -a" "")
           [:unknown :a 4] (with-endings "i -ae -o" "us -a -um" "i -ae -a" "ie(n)s")
           [:unknown :o _] (with-endings "" "us -a -um" "i -ae -a" "ie(n)s")
           [:cardinal :a 1] (with-endings "us" "a" "um")
           [:cardinal :a 2] (with-endings "o" "ae" "o")
           [:cardinal :a 3] (with-endings "es" "es" "ia")
           [:cardinal :a 4] (with-endings "i" "ae" "a")
           [:cardinal :o _] [(:cardinal n)]
           [:ordinal _ _] (with-endings "us" "a" "um")
           [:distributive _ _] (with-endings "i" "ae" "a")
           [_ _ _] [(:cardinal n)]
           )))

(defmethod base-forms :pronoun [noun]
  (let [nnn-endings (fn [m f n]
                      [(str (:nominative noun) m)
                       (str (:nominative noun) f)
                       (str (:nominative noun) n)])
        ngn-endings (fn [m f n]
                      [(str (:nominative noun) m)
                       (str (:genetive noun) f)
                       (str (:nominative noun) n)])]
    (match [(:declension noun) (:variant noun)]
           [:consonant 1] (nnn-endings "ic" "aec" "oc")
           [:consonant 2] (nnn-endings "ic" "aec" "uc")
           [:u 1] (ngn-endings "s" "a" "d")
           [:u 2] (ngn-endings "dem" "adem" "dem")
           [:pronoun 1] (nnn-endings "e" "a" "ud")
           [:pronoun 2] (nnn-endings "e" "a" "um")
           [_ _] nil)))

(defmethod base-forms :verb [verb]
    (letfn [(update-form [forms stem prop end]
              (assoc forms stem (str (get verb prop) end)))
            ]
      (cond (and (= (:conjugation verb) :special)
                 (= (:variant verb) 9))
            [(str (:present verb) " undeclined")]

            (and
             (:type verb :impersonal)
             (= (:present verb) "zzz")
             (= (:infinitive verb) "zzz")
             )
            (-> [nil nil nil nil]
                (update-form 0 :perfect "it")
                (update-form 1 :perfect "isse")
                (update-form 2 :participle "us est")
                (assoc 3 "perfdef"))

            (= (:type verb) :deponent)
            (let [forms (if (:participle verb)
                          [nil nil nil nil]
                          (update-form [nil nil nil nil]
                                       3 :participle "us est"))]
              (match [(:conjugation verb) (:variant verb)]
                     [:a _] (-> forms
                                (update-form 0 :present "or")
                                (update-form 1 :present "ari"))
                     [:e _] (-> forms
                                (update-form 0 :present "eor")
                                (update-form 1 :present "eri"))
                     [:consonant 4] (-> forms
                                        (update-form 0 :present "or")
                                        (update-form 1 :present "iri"))
                     [:consonant _] (-> forms
                                        (update-form 0 :present "or")
                                        (update-form 1 :present "i"))))

            (= (:type verb) :perfectdefinitive)
            (-> [nil nil nil nil]
                (update-form 0 :perfect "i")
                (update-form 1 :perfect "isse")
                (update-form 2 :participle "us"))

            :else
            (let [present (str (:present verb)
                               (match [(:conjugation verb) (:variant verb)]
                                      [:e _] "eo"
                                      [:esse _] "um"
                                      [:defective 2]  "am"
                                      [_ _]  "o"))
                  infinitive (str (:infinitive verb)
                                  (match [(:conjugation verb) (:variant verb)]
                                    [:a _] "are"
                                    [:e _] "ere"
                                    [:consonant 2] "e"
                                    [:consonant 3] "eri"
                                    [:consonant 4] "ire"
                                    [:consonant _] "ere"
                                    [:esse 1] "esse"
                                    [:esse 2] "e"
                                    [:ire 1] "re"
                                    [:ire 2] "le"
                                    [:defective 3] "se"
                                    [:special 1] "are"
                                    [:special 4] "ire"
                                    [:special _] "ere"
                                    [_ _ ] nil))
                  perfect (cond
                            (and (= (:conjugation verb) :esse)
                                 (= (:variant verb) 1))
                            (str (:perfect verb) "i")

                            (not= (:conjugation verb) :special)
                            (str (:perfect verb) "i" (if (and (= (:conjugation verb) :ire)
                                                              (= (:variant verb) 1))
                                                       " (ii)"))

                            :else nil)
                  participle (cond
                               (and (= (:conjugation verb) :esse)
                                    (= (:variant verb) 1))
                               (str (:participle verb) "urus")

                               (= (:type verb) :semideponent)
                               (str (:participle verb) "us sum")

                               (not= (:conjugation verb) :special)
                               (str (:participle verb) "us")

                               :else nil)
                  ]
              [present infinitive perfect participle]))))


(defmethod base-forms :default [w]
  nil)

(defn add-base-forms [w]
  (assoc w :base-forms (base-forms w)))

(defn- load-stem [type word]
  (->  word
       (assoc :speech-part type)
       keywordize-enums
       add-base-forms))

(defn- load-ending [type word]
  (->  word
       (assoc :speech-part type)
       keywordize-enums))

(defonce dictionary (atom {}))


(defn- properties [word & props]
  ((apply juxt props) word))

(defmulti preprocessed-stems base-form-dispatch)

(defmethod preprocessed-stems :simple [word]
  [(:form word)])

(defmethod preprocessed-stems :adjective [adj]
  (properties adj :nominative :genetive :comparative :superlative))

(defmethod preprocessed-stems :adverb [adv]
  (properties adv :positive :comparative :superlative))

(defmethod preprocessed-stems :noun [n]
  (properties n :nominative :genetive))

(defmethod preprocessed-stems :number [n]
  (properties n :cardinal :ordinal :distributive :adverb))

(defmethod preprocessed-stems :pronoun [p]
  (properties p :nominative :genetive))

(defmethod preprocessed-stems :verb [v]
  (properties v :present :infinitive :perfect :participle))

(defmethod preprocessed-stems :default [w]
  nil)

(defn- load-dictionary
  []
  (let [adjectives              (db-load-adjectives @conn {} {} {:row-fn (partial load-stem :adjective)})
        adjective-endings       (db-load-adjective-endings @conn {} {} {:row-fn (partial load-ending :adjective)})
        adverbs                 (db-load-adverbs @conn {} {} {:row-fn (partial load-stem :adverb)})
        adverb-endings          (db-load-adverb-endings @conn {} {} {:row-fn (partial load-ending :adverb)})
        conjunctions            (db-load-conjunctions @conn {} {} {:row-fn (partial load-stem :conjunction)})
        conjunction-endings     (db-load-conjunction-endings @conn {} {} {:row-fn (partial load-ending :conjunction)})
        interjections           (db-load-interjections @conn {} {} {:row-fn (partial load-stem :interjection)})
        interjection-endings    (db-load-interjection-endings @conn {} {} {:row-fn (partial load-ending :interjection)})
        nouns                   (db-load-nouns @conn {} {} {:row-fn (partial load-stem :noun)})
        noun-endings            (db-load-noun-endings @conn {} {} {:row-fn (partial load-ending :noun)})
        numbers                 (db-load-numbers @conn {} {} {:row-fn (partial load-stem :number)})
        number-endings          (db-load-number-endings @conn {} {} {:row-fn (partial load-ending :number)})
        pronouns                (db-load-pronouns @conn {} {} {:row-fn (partial load-stem :pronoun)})
        pronoun-endings         (db-load-pronoun-endings @conn {} {} {:row-fn (partial load-ending :pronoun)})
        verbs                   (db-load-verbs @conn {} {} {:row-fn (partial load-stem :verb)})
        supine-endings          (db-load-supine-endings @conn {} {} {:row-fn (partial load-ending :supine)})
        verbparticilple-endings (db-load-verbparticiple-endings @conn {} {} {:row-fn (partial load-ending :verb-participle)})
        verb-endings            (db-load-verb-endings @conn {} {} {:row-fn (partial load-ending :verb)})
        all-words               (concat adjectives adverbs conjunctions interjections nouns numbers pronouns verbs)
        all-endings             (concat adjective-endings adverb-endings conjunction-endings interjection-endings
                                        noun-endings number-endings pronoun-endings supine-endings
                                        verbparticilple-endings verb-endings)]
    (reset! dictionary
            {
             :adjectives             adjectives
             :adjective-endings      adjective-endings
             :adverbs                adverbs
             :adverb-endings         adverb-endings
             :conjunctions           conjunctions
             :conjunction-endings    conjunction-endings
             :interjections          interjections
             :interjection-endings   interjection-endings
             :nouns                  nouns
             :noun-endings           noun-endings
             :numbers                numbers
             :number-endings         number-endings
             :pronouns               pronouns
             :pronoun-endings        pronoun-endings
             :verbs                  verbs
             :supine-endings         supine-endings
             :verbparticiple-endings verbparticilple-endings
             :verb-endings           verb-endings
             :words                  all-words
             :endings                all-endings
             :stem-lookup (reduce (fn [stems word]
                                    (reduce #(if (contains? %1 %2)
                                               (update-in %1 [%2] conj word)
                                               (assoc %1 %2 #{word}))
                                            stems
                                            (map lookup-stem
                                                 (remove nil? (preprocessed-stems word)))
                                            )
                                    )
                                  {}
                                  (filter preprocessed-stems all-words))
             :unprocessed-stems (remove preprocessed-stems all-words)
             })))

(init-db)
(load-dictionary)
