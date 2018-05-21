(ns circusmaximus.handlers
  (:require [re-frame.core :as re-frame :refer [reg-event-db]]
            [circusmaximus.db :as db]
            ))

(reg-event-db
 :initialize-db
 (fn [_ _]
   db/default-db))

(reg-event-db
 :init-app
 (fn [db]
   db))
