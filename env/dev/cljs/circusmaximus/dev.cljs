(ns ^:figwheel-no-load circusmaximus.dev
  (:require
    [circusmaximus.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
