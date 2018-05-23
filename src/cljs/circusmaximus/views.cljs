(ns circusmaximus.views
  (:require [re-frame.core :as re-frame :refer [dispatch subscribe]]
            [re-com.core :as re-com]
            [circusmaximus.subs :as subs]

            [clojure.string :as str]))

(defn title []
    [re-com/title
     :label (str "Hello world ")
     :level :level1])


(def speech-part-lookup
  { :noun "Noun"
   :adverb "Adverb"
   :conjunction "Conjunction"
   :interjection "Interjection"
   })

(defn wordcase [c]
  [:span.modifier (case c
           :nominative "Nominative"
           :genetive "Genetive"
           :dative "Dative"
           :accusative "Accusative"
           :ablative "Ablative"
           :vocative "Vocative"
           :locative "Locative"
           "Unknown"
           )])

(defn gnumber [n]
  [:span.modifier (case n
           :singular "Singular"
           :plural "Plural"
           "Unknown"
           )])

(defn gender [g]
  [:span (case g
           :masculine "Masculine"
           :feminine "Feminine"
           :neuter "Neuter"
           "Unknown"
           )])

(defn comparison [c]
  [:span (case c
           :positive "Positive"
           :comparative "Comparative"
           :superlative "Superlative"
           (str "Unknown" c)
           )])


(defn declension [d]
  [:span
   (str
    (case d
      :a "a"
      :o "o"
      :consonant "consonant"
      :u "u"
      "Unknown"
      ) " declension")])

(defmulti analysed-details :speech-part)


(defmethod analysed-details :noun [{:keys [endings base-forms] :as n}]
(into  [
   [:div [declension (:declension n)]]
   [:div [gender (:gender n)]]]
   (map (fn [ending]
         [:div [:span.stem (if (and (= (:number ending) :singular)
                                    (= (:word-case ending) :nominative))
                             (:nominative n)
                             (:genetive n))]
          (if-not (str/blank? (:ending ending)) \u2022)
          [:span.ending
           (:ending ending)]
          [wordcase(:wordcase ending)]
          [gnumber (:number ending)]])
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

(defmethod analysed-details :default [{:keys [speech-part] :as w}]
  [[:div (str "Unhandled: " speech-part)]
   [:div (str w)]])


(defn analysed-word [{:keys [speech-part translations base-forms] :as w}]
  (re-com/v-box
   :class (str "analysed-word " (name speech-part))
   :style {:padding "1em"}
   :children (concat
              [[:div.wordtype
                (speech-part-lookup speech-part)]
               [:div (str/join " " base-forms)]]
              (analysed-details w)
              [[:div (get translations "en_US")]
               ])
              ))


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
                       (map analysed-word @result))])))
