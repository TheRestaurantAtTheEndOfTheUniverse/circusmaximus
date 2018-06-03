(ns circusmaximus.handler
  (:require
            [compojure.api.sweet :refer :all]
            [compojure.route :refer [resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [circusmaximus.middleware :refer [wrap-middleware]]
            [circusmaximus.dictionary.analyser :as ana]
            [config.core :refer [env]]
            [schema.core :as s]
            [ring.util.http-response :refer :all]
            [clojure.tools.logging :refer [info]]
            ))

(def mount-target
  [:div#app
      [:h3 "ClojureScript has not been compiled!"]
      [:p "please run "
       [:b "lein figwheel"]
       " in order to start the compiler"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))])

(defn loading-page []
  (html5
    (head)
    [:body {:class "body-container"}
     mount-target
     (include-js "/js/app.js")]))


(defapi app-routes
  (GET "/analyse/:word" []
       :path-params [word :- s/Str]
       (info "Analysing word" word)

       (ok (ana/analyse word))
       )
  (GET "/" [] (loading-page))
  (GET "/about" [] (loading-page))

  (resources "/")
   (not-found "Not Found")
  )

(def app (wrap-middleware #'app-routes))
