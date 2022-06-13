(ns malli-graphql.utils
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            ["graphql" :refer [buildSchema printSchema]]))

(def clean
  "takes string containing graphql schemas parses and outputs using `printSchema`.
   `buildSchema` requires all dependency schemas to be present
  so dependencies are also printed. Removes comments."
  (comp printSchema buildSchema))

(def fs (js/require "fs"))

(defn read-file [path]
  (.readFileSync fs path "utf8"))

(defn write-file [path string]
  (.writeFileSync fs path string))


(defn format-graphql-file [in-files & [out-file]]
  (->> (str/join "\n" (map read-file in-files))
       clean
       (.writeFileSync fs (or out-file (str (last in-files) ".cleaned.graphql")))))

