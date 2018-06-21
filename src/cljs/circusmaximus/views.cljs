(ns circusmaximus.views
  (:require [re-frame.core :as re-frame :refer [dispatch subscribe]]
            [re-com.core :as re-com :refer [v-box h-box box]]
            [circusmaximus.subs :as subs]
            [circusmaximus.wordhelpers :refer [pronoun-stem]]
            [clojure.string :as str]
            [circusmaximus.components.analysedword :refer [analysed-word]]
            [circusmaximus.components.analysedtext :refer [analysed-text]]
            ))



(defn main-panel []
  (let [word (subscribe [:analysed-word])
        result (subscribe [:analysed-result])]
    (fn []
      [re-com/v-box
       :height "100%"
       :children (into [
                        [re-com/input-textarea
                         :rows 10
                         :model (or @word "")
                         :on-change #(dispatch [:analyse %])]
                        (if @result [analysed-text :text @word :analysed @result])]
                       )])))
