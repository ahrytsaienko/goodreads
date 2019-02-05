(ns goodreads.api
  (:require [aleph.http :as http]
            [clojure.xml :as xml]))


(defn response->data [response]
  (->> response
    deref
    :body
    xml/parse))


(defn user-data [api-key user-id]
  (let [url "https://www.goodreads.com/review/list?v=2&key=%s&id=%s"
        response  (http/request
                    {:request-method :get
                     :url            (format url api-key user-id)})]
    (response->data response)))


(defn books-data [api-key books-ids]
  (let [url "https://www.goodreads.com/book/show?key=%s&id=%s"]
    (for [id books-ids
          :let [response (http/request {:request-method :get
                                        :url            (format url api-key id)})]]
      (response->data response))))
