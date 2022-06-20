(ns malli-graphql.ast
  (:require [clojure.walk :refer [postwalk]]
            [malli.core :as m]
            [malli.util :as mu]))

(defn -ref [x] {:$ref x})
(defn -schema [schema {::keys [transform definitions] :as options}]
  (let [derefed (m/deref schema)
        result (merge (transform derefed options) {:schema derefed})
        ref (m/-ref schema)]
    (if ref
      (do (swap! definitions assoc ref result)
          (-ref ref))
      result)))

(defmulti parse-schema-node
  (fn [name _schema _children _options] name)
  :default ::default)

(def int-node {:type :integer})
(def float-node {:type :float})
(def string-node {:type :string})
(def bool-node {:type :boolean})

(defmethod parse-schema-node ::default [_ _ _ _] (throw "not implemented!"))
(defmethod parse-schema-node 'integer? [_ _ _ _] int-node)
(defmethod parse-schema-node 'int? [_ _ _ _] int-node)
(defmethod parse-schema-node 'pos-int? [_ _ _ _] int-node)
(defmethod parse-schema-node 'neg-int? [_ _ _ _] int-node)
(defmethod parse-schema-node 'nat-int? [_ _ _ _] int-node)
(defmethod parse-schema-node 'float? [_ _ _ _] float-node)
(defmethod parse-schema-node 'double? [_ _ _ _] float-node)
(defmethod parse-schema-node 'pos? [_ _ _ _] float-node)
(defmethod parse-schema-node 'neg? [_ _ _ _] float-node)
(defmethod parse-schema-node 'boolean? [_ _ _ _] bool-node)
(defmethod parse-schema-node 'string? [_ _ _ _] string-node)
(defmethod parse-schema-node 'ident? [_ _ _ _] string-node)
(defmethod parse-schema-node 'simple-ident? [_ _ _ _] string-node)
(defmethod parse-schema-node 'qualified-ident? [_ _ _ _] string-node)
(defmethod parse-schema-node 'keyword? [_ _ _ _] string-node)
(defmethod parse-schema-node 'simple-keyword? [_ _ _ _] string-node)
(defmethod parse-schema-node 'qualified-keyword? [_ _ _ _] string-node)
(defmethod parse-schema-node 'symbol? [_ _ _ _] string-node)
(defmethod parse-schema-node 'simple-symbol? [_ _ _ _] string-node)
(defmethod parse-schema-node 'qualified-symbol? [_ _ _ _] string-node)
(defmethod parse-schema-node 'uuid? [_ _ _ _] string-node)
(defmethod parse-schema-node 'uri? [_ _ _ _] string-node)
(defmethod parse-schema-node 'inst? [_ _ _ _] string-node)
(defmethod parse-schema-node 'char? [_ _ _ _] string-node)
(defmethod parse-schema-node 'false? [_ _ _ _] bool-node)
(defmethod parse-schema-node 'true? [_ _ _ _] bool-node)
(defmethod parse-schema-node 'bytes? [_ _ _ _] string-node)
(defmethod parse-schema-node := [_ _ [value] _] {:const value})

(defmethod parse-schema-node :and [_ _ children _]
  (let [non-empty-children (filter (comp not empty?) children)]
    (if-not (empty? non-empty-children)
      {:intersection children} {})))

(defmethod parse-schema-node :or [_ _ children _] {:union children})
(defmethod parse-schema-node :orn [_ _ children _] {:union (map last children)})

(defmethod parse-schema-node ::m/val [_ _ children _] (first children))

(defn- parse-schema-node-map [name schema children options]
  (let [optional (->> children (filter (m/-comp :optional second)) (mapv first))
        object {:type :object
                :properties (apply array-map (mapcat (fn [[k _ s]] [k s]) children))}]
    (if (empty? optional)
      object
      (assoc object :optional optional))))

(defmethod parse-schema-node :map [name schema children options]
  (parse-schema-node-map name (cond-> schema) children options))

(defmethod parse-schema-node :multi [_ _ children _] {:union (mapv last children)})


(defmethod parse-schema-node :vector [_ _ children _] {:type :array, :items (first children)})
(defmethod parse-schema-node :sequential [_ _ children _] {:type :array, :items (first children)})
(defmethod parse-schema-node :enum [_ _ children _] {:enum (map #(array-map :const %) children)})
(defmethod parse-schema-node :maybe [_ _ children _] {:maybe children})
(defmethod parse-schema-node :string [_ schema _ _] string-node)
(defmethod parse-schema-node :int [_ schema _ _] int-node)
(defmethod parse-schema-node :double [_ schema _ _] float-node)
(defmethod parse-schema-node :boolean [_ _ _ _] bool-node)
(defmethod parse-schema-node :keyword [_ _ _ _] string-node)
(defmethod parse-schema-node :qualified-keyword [_ _ _ _] string-node)
(defmethod parse-schema-node :symbol [_ _ _ _] string-node)
(defmethod parse-schema-node :qualified-symbol [_ _ _ _] string-node)
(defmethod parse-schema-node :uuid [_ _ _ _] string-node)

(defmethod parse-schema-node :catn [_ _ children _]
  {:type :catn :items (map (fn [[n _ type]] [n type]) children)})

(defmethod parse-schema-node :cat [_ _ children _]
  {:type :cat :items children})

(defmethod parse-schema-node :=> [_ _ [args ret] _]
  {:type :=> :args args :ret ret})

(defmethod parse-schema-node :function [_ _ children _]
  {:type :function :items children})

(defmethod parse-schema-node :ref [_ schema _ _] (-ref (m/-ref schema)))
(defmethod parse-schema-node :schema [_ schema _ options] (-schema schema options))
(defmethod parse-schema-node ::m/schema [_ schema _ options] (-schema schema options))

(defmethod parse-schema-node :merge [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod parse-schema-node :union [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod parse-schema-node :select-keys [_ schema _ {::keys [transform] :as options}]
  (transform (m/deref schema) options))

(defmethod parse-schema-node :Object [name schema children options]
  (parse-schema-node-map name (m/-set-properties schema (assoc (m/properties schema) :type-name :type)) children options))

(defmethod parse-schema-node :InputObject [name schema children options]
  (parse-schema-node-map name (m/-set-properties schema (assoc (m/properties schema) :type-name :inputtype)) children options))

(defmethod parse-schema-node :ID [_ _ _ _] {:type :ID})

(defn- -ts-schema-walker [schema _ children options]
  (let [m-type (m/type schema)
        actual-schema (case m-type
                        ::m/val (first (m/children schema))
                        schema)]
    (assoc (parse-schema-node m-type schema children options)
           :schema actual-schema)))

(defn- -parse [?schema options] (m/walk ?schema -ts-schema-walker options))

(defn ->ast
  ([?schema]
   (->ast ?schema nil))
  ([?schema options]
   (let [definitions (atom {})
         options (merge options {::m/walk-entry-vals true, ::definitions definitions, ::transform -parse})
         ]
     (cond-> (-parse ?schema options) (seq @definitions) (assoc :definitions @definitions)))))
