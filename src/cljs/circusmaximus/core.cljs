(ns circusmaximus.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [day8.re-frame.http-fx]
            [circusmaximus.handlers :as cmh]
            [circusmaximus.views :as views]
              ))


(defn mount-root []
  (reagent/render [views/main-panel] (.getElementById js/document "app")))

(defn init! []
  (re-frame/dispatch-sync [:initialize-db])
  (re-frame/dispatch [:init-app])
  (mount-root))
