(ns circusmaximus.handlers
  (:require [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx]]
            [circusmaximus.db :as db]
            [ajax.core :as ajax]
            [cognitect.transit :as transit]
            [cljs-time.core :as time]
            )
     (:import [goog.date UtcDateTime]))

(def transit-readers
  {:handlers
   {"m" (transit/read-handler (fn [s] (UtcDateTime.fromTimestamp s)))}})

(def resp-format
  (ajax/transit-response-format
    {:reader (transit/reader :json transit-readers)}))

(def transit-writers
  {:handlers
   {UtcDateTime (transit/write-handler
                  (constantly "m")
                  (fn [v] (.getTime v))
                  (fn [v] (str (.getTime v))))}})

(def req-format
  (ajax/transit-request-format
    {:writer (transit/writer :json transit-writers)}))

(reg-event-db
 :initialize-db
 (fn [_ _]
   db/default-db))

(reg-event-db
 :init-app
 (fn [db]
   db))

(reg-event-db
 :word-analysed-successfully
 (fn [db [_ result]]
   (assoc-in db [:analysed-word :result] result)))

(reg-event-db
 :word-analyse-failed
 (fn [db result]
   (println result)
   db))

(reg-event-fx
 :analyse
 (fn [{:keys [db]} [_ word]]
   { :db (assoc-in db [:analysed-word :word] word)
    :http-xhrio {:method          :get
                 :uri             (str "/analyse/" word)
                 :response-format resp-format
                 :on-success      [:word-analysed-successfully]
                 :on-failure      [:word-analyse-failed]}}))
