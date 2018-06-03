(ns circusmaximus.views
  (:require [re-frame.core :as re-frame :refer [dispatch subscribe]]
            [re-com.core :as re-com :refer [v-box h-box box]]
            [circusmaximus.subs :as subs]

            [clojure.string :as str]))

(defn title []
    [re-com/title
     :label (str "Hello world ")
     :level :level1])


(def speech-part-lookup
  { :noun        "Noun"
   :number       "Number"
   :adverb       "Adverb"
   :adjective    "Adjective"
   :conjunction  "Conjunction"
   :interjection "Interjection"
   :preposition  "Preposition"
   :verb         "Verb"
   })

(def voice-lookup
  {:active  "Active"
   :passive "Passive"})

(def tense-lookup
  {:present "Present"
   :imperfect "Imperfect"
   :future "Future"
   :perfect "Perfect"
   :plusquamperfect "Plusquamperferct"
   :futureperfect "Future perfect"})

(def conjugation-lookup
  {:participle "Participle"
   :a "A"
   :e "E"
   :consonant "Consonant"
   :i "I"
   :esse "Special (esse)"
   :ire "Special (ire)"
   :defective "Defective"
   :special "Special"})

(def mood-lookup
  {
   :indicative "Indicative"
   :subjunctive "Subjunctive"
   :imperative "Imperative"
   :infinitive "Infinitive"
   :participle "Participle"
   :supine "Supine"
   })

(def pers-lookup
  {
   1 "First person"
   2 "Second person"
   3 "Third person"
   })

(defn modifier [lookup val]
  [:span.modifier (lookup val)])

(defn voice [v]
  [modifier voice-lookup v])

(defn tense [e]
  (cond (map? e) [modifier tense-lookup (:tense e)]
        (keyword? e) [modifier tense-lookup e]
        :else (str "Unhandled " e)))

(defn conjugation [c]
  [:span (str (conjugation-lookup c) " conjugation")])

(defn mood [e]
  [modifier mood-lookup (:mood e)]
  )

(defn person [e]
  [modifier pers-lookup (:person e)])

(defn wordcase [c]
  (if-not (= c :unknown)
    [:span.modifier (case c
                      :nominative "Nominative"
                      :genetive   "Genetive"
                      :dative     "Dative"
                      :accusative "Accusative"
                      :ablative   "Ablative"
                      :vocative   "Vocative"
                      :locative   "Locative"
                      (str "Unknown case " c)
                      )]))

(defn gnumber [n]
  (if-not (= n :unknown)
    [:span.modifier (case n
                      :singular "Singular"
                      :plural   "Plural"
                      (str "Unknown number " n)
                      )]))

(defn gender [g]
  (if-not (= g :unknown)
  [:span.modifier (case g
           :masculine "Masculine"
           :feminine  "Feminine"
           :neuter    "Neuter"
           (str "Unknown gender" g)
           )]))

(defn comparison [c]
  [:span (case c
           :positive    "Positive"
           :comparative "Comparative"
           :superlative "Superlative"
           (str "Unknown comparison" c)
           )])

(defn number-type [n]
  [:span.modifier (case n
           :cardinal    "Cardinal"
           :ordinal "Ordinal"
           :distributive "Distributive"
           :adverb "Adverb"
           (str "Unknown number" n)
           )])


(defn declension [d]
  [:span
   (str
    (case d
      :a         "a"
      :o         "o"
      :consonant "consonant"
      :u         "u"
      "Unknown"
      ) " declension")])

(defmulti analysed-details :speech-part)


(defmethod analysed-details :noun [{:keys [endings base-forms] :as n}]
(into  [
   [:div [declension (:declension n)]]
        [:div [gender (:gender n)]]
        ]
   (map (fn [ending]
         [:div [:span.stem (if (and (= (:number ending) :singular)
                                    (or (= (:wordcase ending) :nominative)
                                        (= (:wordcase ending) :vocative)
                                        (and (= (:wordcase ending) :accusative)
                                             (= (:gender n) :neuter)))
                                        )
                             (:nominative n)
                             (:genetive n))]
          (if-not (str/blank? (:ending ending)) \u2022)
          [:span.ending
           (:ending ending)]
          [wordcase(:wordcase ending)]
          [gnumber (:number ending)]])
       endings)))

(defmethod analysed-details :adjective [{:keys [endings base-forms] :as n}]
  (into  [[declension (:declension n)]]
         (map (fn [ending]
                [:div
                 [:span.stem (if (and (= (:number ending) :singular)
                                    (or (= (:wordcase ending) :nominative)
                                        (= (:wordcase ending) :vocative)
                                        (and (= (:wordcase ending) :accusative)
                                             (= (:gender n) :neuter))))
                                    (:nominative n)
                                    (:genetive n))]
                 (if-not (str/blank? (:ending ending)) \u2022)
                 [:span.ending
                  (:ending ending)]
                 [wordcase(:wordcase ending)]
                 [gnumber (:number ending)]
                 [gender (:gender ending)]
                 ])
       endings)))

(defmethod analysed-details :number [{:keys [endings base-forms] :as n}]
  (into  [[declension (:declension n)]
          [:div (str (:numbervalue n))]]
         (map (fn [ending]
                [h-box
                 :children [
                            [:soan (n (:type ending))]
                            (if-not (str/blank? (:ending ending)) [:span.stemsep "\u2022"])
                            [:span (:ending ending)]
                            [gnumber (:number ending)]
                            [wordcase (:wordcase ending)]
                            [gender (:gender ending)]
                            [number-type (:type ending)]
                            ]])
              endings)))

(defmethod analysed-details :adverb [{:keys [endings] :as a}]
   (map (fn [ending]
          [:div [:span.stem (get a (:comparison ending))]
           (if-not (str/blank? (:ending ending)) \u2022)
           [:span.ending
            (:ending ending)]
           [comparison (:comparison ending)]
           ])
        endings))

(defmethod analysed-details :conjunction [{:keys [speech-part] :as w}]
)

(defmethod analysed-details :interjection [{:keys [speech-part] :as w}]
)

(defmethod analysed-details :preposition [{:keys [endings base-forms] :as n}]
          (map (fn [ending]
                 [:div [:span.stem (:form n)]
                  (if-not (str/blank? (:ending ending)) \u2022)
                  [:span.ending
                   (:ending ending)]
                  [wordcase(:wordcase ending)]
                  ])
               endings))


(defmethod analysed-details :verb [{:keys [endings base-forms] :as v}]
  (into [[:div (conjugation (:conjugation v))]]
        (map (fn [ending]
               (let [stem (if (= (:speech-part ending) :supine)
                              (:participle v)
                              (v (:verbstem ending)))
                     esse-ending (:esse-ending ending)
                     ]
                 [:table {:style {:border-collapse "collapse"}}
                  [:tbody
                   [:tr
                    [:td
                     [:span.stem stem]
                     (if-not (or
                              (str/blank? stem)
                              (str/blank? (:ending ending)))
                       \u2022)
                     [:span.ending (:ending ending)]]
                    [:td
                     (case (:speech-part ending)
                       :verb-participle [:span (wordcase (:wordcase ending))
                                         (gnumber (:number ending))
                                         (gender (:gender ending))
                                         (tense ending)
                                         [:span.modifier "Participle"]
                                         (voice (:voice ending))
                                         ]
                       :verb [:span
                              (mood ending)
                              (if (not= (:mood ending) :infinitive)
                                (person ending))
                              (gnumber (:number ending))]
                       :supine [:span
                                (wordcase (:wordcase ending))
                                (gnumber (:number ending))
                                (gender (:gender ending))
                                (tense ending)
                                [:span.modifier "Supine"]
                                (voice (:voice ending))
                                ]
                       [:div "Not handled " (:speech-part ending)
                        (str ending)
                        ])]]
                   (if (:esse-ending ending)
                     [:tr [:td]
                      [:td
                       (person esse-ending)
                       (tense (case (:tense esse-ending)
                                :present :perfect
                                :imperfect :plusquamperfect
                                :future :futureperfect
                                (str "tense" (:tense esse-ending))))
                       (mood esse-ending)
                       ]]
                     )
                   ]]))
             endings)))

(defmethod analysed-details :default [{:keys [speech-part] :as w}]
  [[:div (str "Unhandled: " speech-part)]
   [:div (str w)]])


(defn analysed-word [{:keys [speech-part translations base-forms] :as w}]
  (re-com/v-box
   :class (str "analysed-word " (name speech-part))
   :children (concat
              [[:div.wordtype
                (speech-part-lookup speech-part)]
               [:div (str/join ", " (map #(str/replace % "zzz" "-")
                                         (remove nil? base-forms)))]]
              (analysed-details w)
              [[:div.translation (get translations "en_US")]
               ])
   )
  )


(defn main-panel []
  (let [word (subscribe [:analysed-word])
        result (subscribe [:analysed-result])
        ]

    (fn []
      [re-com/v-box
       :height "100%"
       :children (into [[title]
                        [re-com/input-text
                         :model (or @word "")
                         :on-change #(dispatch [:analyse %])]]
                       (map analysed-word @result))]

      )))
