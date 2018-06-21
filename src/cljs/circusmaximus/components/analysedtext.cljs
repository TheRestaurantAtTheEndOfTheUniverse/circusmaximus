(ns circusmaximus.components.analysedtext
  (:require [re-com.core :as re-com :refer [v-box h-box box]]
            [reagent.core :as r]
            [circusmaximus.components.analysedword :refer [analysed-word]]
            [clojure.string :as str]
            ))


(defn- is-word? [s]
  (and (string? s)
       (not (nil? (re-matches #"^[a-zA-Z]+$" s)))))

(defn- is-letter? [s]
  (not (nil? (re-matches #"[a-zA-Z]" s))))

(defn- split-text [text]
  (map #(apply str %) (partition-by  is-letter? text))
  )

(defn analysed-text [& {:keys [text analysed] :as params}]
  (let [selected-word (r/atom nil)]
    (fn [& {:keys [text analysed] :as params}]
      [re-com/v-box
       :children (concat
                  [(into
                   [:div {:style {:width "40em"
                                  :background-color "white"
                                  :margin-bottom "1em"}}]
                   (loop [parts (split-text text)
                          acc   []]
                     (let [current        (first parts)
                           on-word?       (is-word? current)
                           follow-up-word (if on-word?
                                            (first (filter is-word? (rest parts))))]
                       (if (seq parts)
                         (recur (rest parts)
                                (conj acc [:span (merge {}
                                                        (if on-word?
                                                          {:on-click #(reset! selected-word
                                                                              (if follow-up-word
                                                                                [(str/lower-case current)
                                                                                 (str/lower-case (str current " " follow-up-word))]
                                                                                [(str/lower-case current)])
                                                                              )}))
                                           current]))
                         acc
                         ))))]
                  (map analysed-word (apply concat (vals (select-keys analysed @selected-word)))))])))
