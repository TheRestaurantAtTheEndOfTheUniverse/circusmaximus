(ns circusmaximus.dictionary.dictschema
  (:require [schema.core :as s])
  )

(def speech-part (s/enum :adjective :adverb :conjunction :interjection :noun :number
                         :pronoun :verb :supine :verb-participle))

(def declension (s/enum :a :e :o :consonant :u :indeclinable :comparison :pronoun))

(def age (s/enum :archaic :early :classic :late :later :medieval :neo :modern :graffiti :unknown))

(def comparison (s/enum :positive :comparative :superlative :unknown))

(def word-frequency (s/enum :very-frequent :frequent :common :lesser :uncommon :very-rare :inscription :graffiti :pliny :unknown))

(def gender (s/enum :masculine :feminine :neuter :common :unknown))

(def tense (s/enum :present :imperfect :future :perfect :plusquamperfect :futureperfect :unknown))

(def nountype (s/enum :singularonly :pluralonly :abstractidea :propername :locale :person :thing :where :unknown))

(def declcase (s/enum :nominative :genetive :dative :accusative :ablative :vocative :locative :unknown))

(def mood (s/enum :indicative :subjunctive :imperative :infinitive :participle :supine :unknown))

(def grammaticalnumber (s/enum :singular :plural :unknown))

(def conjugation (s/enum :participle :a :e :consonant :i :esse :ire :defective :special))

(def numbertype (s/enum :cardinal :ordinal :distributive :adverb :unknown))

(def pronountype (s/enum :personal :relative :reflexive :demonstrative :interrogative :indefinitive :adjective :unknown))

(def verbtype (s/enum :unknown :esse :esse_compound :genetive :dative :ablative
                      :transitive :intransitive :impersonal :deponent :semideponent :perfectdefinitive))

(def voice (s/enum :active :passive  :unknown))

(def wordcase (s/enum :nominative :genetive :dative :accusative :ablative :vocative :locative :unknown))

(def verbstem (s/enum :present :infinitive :perfect :participle))


(def default-entries
  {:id          s/Int
   :age          age
   :frequency    (s/maybe word-frequency)
   :speech-part speech-part
   })

(def default-word-entries
  (merge default-entries
         {:translations {(s/constrained s/Str #(re-matches #"[a-z][a-z]_[A-Z][A-Z]" %)) s/Str}
          :base-forms (s/maybe [s/Str])}))

(def default-ending-entries
  (merge default-entries
         {:ending s/Str}))

(def adjective
  (merge {
          :nominative  s/Str
          :genetive    (s/maybe s/Str)
          :comparative (s/maybe s/Str)
          :superlative (s/maybe s/Str)
          :comparison  comparison
          :declension  declension
          :variant     s/Int
          }
         default-word-entries))

(def adjective-ending
  (merge {
          :comparison comparison
          :gender gender
          :number grammaticalnumber
          :declension declension
          :variant s/Int
          :wordcase wordcase
          }
         default-ending-entries))

(def adverb
  (merge {:positive s/Str
          :comparative (s/maybe s/Str)
          :superlative (s/maybe s/Str)
          :comparison (s/maybe comparison)
          }
         default-word-entries))

(def adverb-ending
  (merge {:comparison comparison}
         default-ending-entries))

(def conjunction
  (merge {:form s/Str}
         default-word-entries))

(def conjunction-ending
  default-ending-entries)

(def interjection
  (merge {:form s/Str}
         default-word-entries))

(def interjection-ending
  default-ending-entries)

(def noun
  (merge {:nominative s/Str
          :genetive (s/maybe s/Str)
          :gender (s/maybe gender)
          :declension declension
          :variant s/Int
          :nountype (s/maybe nountype)}
         default-word-entries))

(def noun-ending
  (merge {:gender gender
          :number grammaticalnumber
          :declension declension
          :variant s/Int
          :wordcase wordcase}
         default-ending-entries))

(def numberword
  (merge {:cardinal (s/maybe s/Str)
          :ordinal (s/maybe s/Str)
          :distributive (s/maybe s/Str)
          :adverb (s/maybe s/Str)
          :numbervalue s/Int
          :declension declension
          :variant s/Int
          }
         default-word-entries))

(def number-ending
  (merge {:type numbertype
          :gender gender
          :number grammaticalnumber
          :declension declension
          :variant s/Int
          :wordcase wordcase}
         default-ending-entries))

(def preposition
  (merge {:form s/Str
          :wordcase wordcase}
         default-word-entries))

(def preposition-ending
  (merge {:wordcase wordcase}
         default-ending-entries))

(def pronoun
  (merge {:type pronountype
          :nominative s/Str
          :genetive s/Str
          :gender (s/maybe gender)
          :declension declension
          :variant s/Int
          :nountype (s/maybe nountype)}
         default-word-entries))

(def pronoun-ending
  (merge {:gender gender
          :number grammaticalnumber
          :declension declension
          :variant s/Int
          :wordcase wordcase}
         default-ending-entries))

(def verb
  (merge {:present (s/maybe s/Str)
          :infinitive (s/maybe s/Str)
          :perfect (s/maybe s/Str)
          :participle (s/maybe s/Str)
          :conjugation conjugation
          :variant s/Int
          :type verbtype}
         default-word-entries))

(def verb-ending
  (merge {:voice voice
          :tense tense
          :mood mood
          :conjugation conjugation
          :variant s/Int
          :verbstem verbstem
          :person s/Int
          :number grammaticalnumber}
         default-ending-entries))

(def verbparticiple-ending
  (merge {:voice       voice
          :tense       tense
          :conjugation conjugation
          :variant     s/Int
          :verbstem    verbstem
          :gender      gender
          :wordcase    wordcase
          :number      grammaticalnumber}
         default-ending-entries))

(def supine-ending
  (merge {:conjugation conjugation
          :variant     s/Int
          :wordcase    wordcase
          :gender      gender
          :number      grammaticalnumber}
         default-ending-entries))


(def word
  (s/conditional #(= (:speech-part %) :adjective) adjective
                 #(= (:speech-part %) :adverb) adverb
                 #(= (:speech-part %) :conjunction) conjunction
                 #(= (:speech-part %) :interjection) interjection
                 #(= (:speech-part %) :noun) noun
                 #(= (:speech-part %) :number) numberword
                 #(= (:speech-part %) :preposition) preposition
                 #(= (:speech-part %) :pronoun) pronoun
                 #(= (:speech-part %) :verb) verb
                 ))

(def ending
  (s/conditional #(= (:speech-part %) :adjective) adjective-ending
                 #(= (:speech-part %) :adverb) adverb-ending
                 #(= (:speech-part %) :conjunction) conjunction-ending
                 #(= (:speech-part %) :interjection) interjection-ending
                 #(= (:speech-part %) :noun) noun-ending
                 #(= (:speech-part %) :number) number-ending
                 #(= (:speech-part %) :preposition) preposition-ending
                 #(= (:speech-part %) :pronoun) pronoun-ending
                 #(= (:speech-part %) :verb) verb-ending
                 #(= (:speech-part %) :verb-participle) verbparticiple-ending
                 #(= (:speech-part %) :supine) supine-ending
                 ))


(def word-list [word])
(def ending-list [ending])

(def analysed-word
  (s/conditional #(= (:speech-part %) :adjective) (assoc adjective
                                                         :endings [adjective-ending])
                 #(= (:speech-part %) :adverb) adverb
                 #(= (:speech-part %) :conjunction) conjunction
                 #(= (:speech-part %) :interjection) interjection
                 #(= (:speech-part %) :noun) (assoc noun
                                                    :endings [noun-ending])
                 #(= (:speech-part %) :number) numberword
                 #(= (:speech-part %) :preposition) preposition
                 #(= (:speech-part %) :pronoun) pronoun
                 #(= (:speech-part %) :verb) (assoc verb
                                                    :endings [(s/conditional #(= (:speech-part %) :verb) verb-ending
                                                                           #(= (:speech-part %) :verb-participle) verbparticiple-ending
                                                                           #(= (:speech-part %) :supine) supine-ending)])))

(def analysed-words [analysed-word])
