(ns malli-graphql.utils
  (:require ["graphql" :refer [buildSchema printSchema]]))

;; removes comments
(def clean (comp printSchema buildSchema))

(def fs (js/require "fs"))

(defn read-file [path]
  (.readFileSync fs path "utf8"))

(defn write-file [path string]
  (.writeFileSync fs path string))


(defn format-graphql-file [in-file & [out-file]]
  (->> (read-file in-file)
       clean
       (.writeFileSync fs (or out-file (str in-file ".cleaned.graphql")))))
