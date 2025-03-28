(ns pez.url-short
  (:require [clojure.string :as str]
            [promesa.core :as p]))

(defn- generate-code []
  (str/join "" (take 6 (repeatedly #(rand-nth "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))))

(defn- shorten-url [url ^js env]
  (p/catch
   (p/let [short-code (generate-code)
           timestamp (js/Date.now)
           ^js stmt (.prepare (.-DB env) "INSERT INTO urls (short_code, long_url, created_at) VALUES (?, ?, ?)")
           ^js result (.bind stmt short-code url timestamp)
           _ (.run result)]
     short-code)
   #(js/console.error "Error inserting to D1:" %)))

(defn- handle-request [request ^js env _ctx]
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
                  short-url (str (.-origin url) "/" short-code)]
            (js/Response. (js/JSON.stringify #js {:shortUrl short-url})
                          #js {:status 200 :headers #js {"Content-Type" "application/json"}}))
          (js/Response. "Invalid URL" #js {:status 400})))

      (and (= method "GET") (= pathname "/"))
      (p/resolved (js/Response. "Hello World Workers, Unite!"
                                #js {:status 200 :headers #js {"Content-Type" "text/plain"}}))

      :else
      (p/let [code (subs pathname 1)
              ^js stmt (.prepare (.-DB env) "SELECT long_url FROM urls WHERE short_code = ?")
              ^js result (.bind stmt code)
              ^js row (.first result)
              ^js long-url (.-long_url row)]
        (if long-url
          (js/Response. nil #js {:status 301 :headers #js {"Location" long-url}})
          (js/Response. "Not Found" #js {:status 404}))))))

(def ^:export handler
  #js {:fetch (fn [req env ctx]
                (p/catch
                 (handle-request req env ctx)
                 #(js/Response. (str "Error processing request: " %)
                                #js {:status 500})))})