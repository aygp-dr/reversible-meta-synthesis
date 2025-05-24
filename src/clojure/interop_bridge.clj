(ns reversible-meta-synthesis.interop-bridge
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]
            [reversible-meta-synthesis.reversible-interpreter :as ri]
            [reversible-meta-synthesis.ebg :as ebg]))

;; Convert a program to JSON
(defn export-program [clauses]
  (mapv clause->json clauses))

;; Convert JSON back to a program
(defn import-program [json-data]
  (mapv json->clause json-data))

;; Convert a clause to JSON
(defn clause->json [[head body]]
  {:head (term->json head)
   :body (mapv term->json body)})

;; Convert JSON back to a clause
(defn json->clause [{:keys [head body]}]
  [(json->term head)
   (mapv json->term body)])

;; Convert a term to JSON
(defn term->json [term]
  (cond
    (ri/variable? term) {:type "var" :name (name term)}
    (ri/constant? term) {:type "const" :name (subs (name term) 1)}
    (symbol? term) {:type "atom" :value (name term)}
    (string? term) {:type "string" :value term}
    (number? term) {:type "number" :value term}
    (sequential? term) {:type "list" :value (mapv term->json term)}
    :else {:type "unknown"}))

;; Convert JSON back to a term
(defn json->term [{:keys [type] :as json-term}]
  (case type
    "var" (symbol (str "*" (:name json-term)))
    "const" (symbol (str "/" (:name json-term)))
    "atom" (symbol (:value json-term))
    "string" (:value json-term)
    "number" (:value json-term)
    "list" (mapv json->term (:value json-term))
    nil))

;; Convert an explanation tree to JSON
(defn export-explanation [explanation]
  (explanation->json explanation))

;; Convert JSON back to an explanation tree
(defn import-explanation [json-data]
  (json->explanation json-data))

;; Convert explanation tree to JSON
(defn explanation->json [{:keys [goal children]}]
  {:goal (term->json goal)
   :children (mapv explanation->json children)})

;; Convert JSON back to explanation tree
(defn json->explanation [{:keys [goal children]}]
  (ebg/->ExplanationNode
    (json->term goal)
    (mapv json->explanation children)))

;; Call external implementation via HTTP
(defn call-external [language function input]
  (let [url (str "http://localhost:8080/api/" language "/" function)
        response (http/post url 
                            {:body (json/write-str input)
                             :content-type :json
                             :accept :json})]
    (when (= (:status response) 200)
      (json/read-str (:body response) :key-fn keyword))))
