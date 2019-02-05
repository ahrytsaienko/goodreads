(ns goodreads.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [manifold.deferred :as d]
            [goodreads.api :as api]
            [clojure.set :as set]))

;; helpers

(defn find-node [k coll]
  (some #(when (= k (:tag %)) %) coll))

(def get-val (comp first :content find-node))

(defn filter-nodes [k coll]
  (filter #(= k (:tag %)) coll))

(def collect-content (comp #(mapcat :content %) filter-nodes))


;; build recommendations

(defn parse-shelves [content]
  (keep #(when (= :shelves (:tag %))
           (-> % :content first :attrs :name)) content))


(defn parse-book-ids [content]
  (->> content
    (collect-content :book)
    (collect-content :id)))


(defn parse-authors [content]
  (->> content
    (get-val :authors)
    :content
    (filter-nodes :name)
    (map #(hash-map :name (-> % :content first)))))


(defn prepare-similar-books [books]
  (map (fn [{:keys [content]}]
         {:id             (get-val :id content)
          :title          (get-val :title content)
          :link           (get-val :link content)
          :average-rating (Float/parseFloat (get-val :average_rating content))
          :authors        (parse-authors content)}) books))


(defn parse-book-info [{:keys [content]}]
  (->> content
    (collect-content :book)
    (collect-content :similar_books)
    (prepare-similar-books)))


(defn find-similar-books [api-key book-ids]
  (let [books (api/books-data api-key book-ids)]
    (mapcat parse-book-info books)))


(defn remove-ignored-books [book-ids similar-books]
  (remove #(get book-ids (:id %)) similar-books))


(defn book-ids-by-shelve-type [book-ids shelves shelve-type]
  (->> (map #(when (= %1 shelve-type) %2) book-ids shelves)
    (remove nil?)
    (set)))


(defn build-recommendations [{:keys [api-key user-id number-books] :as options}]
  (d/success-deferred
    (let [data             (api/user-data api-key user-id)
          reviews          (->> (:content data) (find-node :reviews) :content (mapcat :content))
          shelves          (parse-shelves reviews)
          book-ids         (parse-book-ids reviews)
          read-book-ids    (book-ids-by-shelve-type book-ids shelves "read")
          reading-book-ids (book-ids-by-shelve-type book-ids shelves "currently-reading")
          ignored-books    (set/union read-book-ids reading-book-ids)]
      (->> read-book-ids
        (find-similar-books api-key)
        (remove-ignored-books ignored-books)
        (sort-by :average-rating)
        (reverse)
        (take number-books)))))


(def cli-options [["-u"
                   "--user-id"
                   "Recommended books for user"
                   :required "user-id"
                   :parse-fn #(Integer/parseInt %)
                   :missing "Please, specify user's id"]
                  ["-n"
                   "--number-books"
                   "How many books do you want to recommend"
                   :default 10
                   :parse-fn #(Integer/parseInt %)]
                  ["-t"
                   "--timeout-ms"
                   "Wait before finished"
                   :default 5000
                   :parse-fn #(Integer/parseInt %)]
                  ["-h" "--help"]])


(defn book->str [{:keys [title link average-rating authors]}]
  (format "\"%s\", rating: %s, by %s\nMore: %s"
    title
    average-rating
    (->> authors
      (map :name)
      (clojure.string/join ", "))
    link))


(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary) (System/exit 0))
      (some? errors) (do (println errors) (System/exit 1))
      (empty? args) (do (println "Please, specify user's token") (System/exit 1))
      :else (let [books (-> (build-recommendations (assoc options :api-key (first args)))
                          (d/timeout! (:timeout-ms options) ::timeout)
                          deref)]
              (cond
                (= ::timeout books) (println "Not enough time :(")
                (empty? books) (println "Nothing found.")
                :else (doseq [[i book] (map-indexed vector books)]
                        (println (str "#" (inc i)))
                        (println (book->str book))
                        (println)))))))
