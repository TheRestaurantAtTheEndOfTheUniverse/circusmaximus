(ns circusmaximus.views
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [circusmaximus.subs :as subs]
            ))

(defn title []
    [re-com/title
     :label (str "Hello world ")
     :level :level1])

(defn main-panel []
  [re-com/v-box
   :height "100%"
   :children [[title]]])
