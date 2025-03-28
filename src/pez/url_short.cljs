(ns pez.url-short
  (:require [clojure.string :as str]
            [promesa.core :as p]))

(defn- generate-code []
  (str/join "" (take 6 (repeatedly #(rand-nth "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))))

(defn- shorten-url [url ^js env]
  (p/catch
   (p/let [short-code (generate-code)
           _ (js/console.log "Attempting to put:" short-code "->" url)
           _result+ (.put (.-SHORT_URLS env) short-code url #js {:expirationTtl 2592000})
           _ (js/console.log "Put succeeded:" short-code "->" url)]
     short-code)
   #(js/console.error "Error putting to KV:" %)))

(defn- handle-request [request ^js env ctx]
  (let [url (js/URL. (.-url request))
        pathname (.-pathname url)
        method (.-method request)]
    (cond
      (and (= method "POST") (= pathname "/shorten"))
      (p/let [body-text (.text request)
              params (js/URLSearchParams. body-text)
              long-url (.get params "url")]
        (if (and long-url (re-matches #"https?://.+" long-url))
          (p/let [short-code (shorten-url long-url env)
                  short-url (str (.-origin url) "/" short-code)
                  _ (js/console.log "Returning short URL:" short-url)]
            (js/Response. (js/JSON.stringify #js {:shortUrl short-url})
                          #js {:status 200
                               :headers #js {"Content-Type" "application/json"}}))
          (js/Response. "Invalid URL" #js {:status 400})))

      (and (= method "GET") (str/starts-with? pathname "/") (not= pathname "/"))
      (p/let [code (subs pathname 1)
              _ (js/console.log "Get:" code)
              result (.get (.-SHORT_URLS env) code)
              _ (js/console.log "Got:" result)]
        (if result
          (js/Response. nil #js {:status 301 :headers #js {"Location" result}})
          (js/Response. "Not Found" #js {:status 404})))

      :else
      (p/resolved (js/Response. "Hello World Workers, Unite!"
                                #js {:status 200
                                     :headers #js {"Content-Type" "text/plain"}})))))

(def ^:export handler
  #js {:fetch (fn [req env ctx]
                (p/catch
                 (handle-request req env ctx)
                 #(js/Response. (str "Error processing request: " %)
                                #js {:status 500})))})