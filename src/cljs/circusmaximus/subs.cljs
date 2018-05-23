(ns circusmaximus.subs
  (:require [re-frame.core :as re-frame :refer [reg-sub]]))

(reg-sub
 :analysed-word
 (fn [db]
   (get-in db [:analysed-word :word])))

(reg-sub
 :analysed-result
 (fn [db]
   (get-in db [:analysed-word :result])))
