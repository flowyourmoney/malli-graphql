(ns malli-graphql.core
  (:require
   #?@(:clj [[clojure.core :as s-lib]
             [clojure.java.io :as io]]
       :cljs [[cljs.nodejs :as nodejs]
              [goog.string :as s-lib]
              [goog.string.format]
              [cljs.tools.reader.edn :refer [read-string]]
              [malli-graphql.utils :as utils :refer [clean read-file]]])
   [clojure.walk :refer [postwalk]]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [malli.core :as m]
   [malli.registry :as mr]
   [camel-snake-kebab.core :as csk]
   [malli-graphql.ast :refer [->ast]])
  #?(:clj (:import java.io.PushbackReader)))

(def graphql-type->schema
  {:Object (m/-map-schema)
   :InputObject (m/-map-schema)
   :Schema (m/-map-schema)
   :Union (m/-or-schema)
   :Enum (m/-enum-schema)
   :List (m/-collection-schema {:type :vector, :pred vector?, :empty []})
   :Int (m/-int-schema)
   :Float (m/-double-schema)
   :String (m/-string-schema)
   :Boolean (m/-boolean-schema)
   :ID (m/-simple-schema {:type :ID, :pred string?})})

(defn graphql-type->malli-schema
  ([schema-kw]
   graphql-type->malli-schema schema-kw nil)
  ([schema-kw & [{:keys [convert-custom?]}]]
   (letfn [(err-missing-schema [schema-kw]
             (throw (ex-info (str (name schema-kw) " does not have an malli built-in equivalent") {})))]
     (case schema-kw
       :Object :map
       :InputObject :map
       :Schema :map
       :Union :or
       :Enum :enum
       :List :vector
       :Int :int
       :Float :double
       :String :string
       :Boolean :boolean
       :ID (if convert-custom?
             (err-missing-schema :ID)
             :ID)
       schema-kw))))

(def gql-type-attributes
  {::object {:type-name "type"}
   ::input-object {:type-name "input"}
   ::gql-schema {:type-name "schema"}})

(defn- not-implemented! [msg & [map]]
  (throw (ex-info msg (or map {}))))

(defn set-gql-registry! []
  (mr/set-default-registry!
   (mr/composite-registry
    (mr/fast-registry (m/default-schemas))
    graphql-type->schema)))

;; additional fields included in G-AST node after transformation
(def Node [:map
           [:const-val [:or :keyword :string]]
           [:kind [:enum :fn :fn-entry]]])

;; graphql node context is inherited by the parent
(def Context [:map
              [:depth :int]
              [:root-name :string]])

(def SupportedProperties [:maybe
                          [:map
                           [:type-name {:optional true} :string]
                           [:extend {:optional true} :any]]])

(defn- -|single-val [children]
  (let [one-tuple [:cat any?]]
    (m/validate one-tuple children)
    (first children)))

(defn- -|pair-vals [children]
  (let [two-tuple [:cat any? any?]]
    (m/validate two-tuple children)
    children))

(def ^:dynamic *key-val-separator* ":")
(def ^:dynamic *inline-separator* " ")
(def ^:dynamic *line-separator* "\n")
(def ^:dynamic *undefined-type-name* "UNDEFINED")

;; TODO implement unnesting of nested malli schemas to remove the need for this
(def ^:private supported-depth-opts {:min 0 :max 1})
(def depth-validator (m/validator [:int supported-depth-opts]))

(defn- check-depth! [depth & [map]]
  (if (depth-validator depth)
    depth
    (not-implemented! "Unnesting of malli schemas not supported" {:info map,
                                                                  :depth depth})))

(defn- -|gnode-depth [gnode]
  (check-depth! (get-in gnode [:context :depth])))

(def nullable? (comp boolean :nullable :node))

(defn- ->nullable [gnode]
  (assoc-in gnode [:node :nullable] true))

(defn- ->opt [gnode]
  (assoc-in gnode [:options :optional] true))

(defn- update-children [gnode f] (update gnode :children (partial map f)))

(defn- -|gnode-props
  [{:keys [options node]}]
  (let [{:keys [any-props?]} options
        schema (:schema node)
        props (when schema (m/properties schema))]
    (if (or any-props?
            (m/validate SupportedProperties props))
      props
      (throw (ex-info "schema had un-supported properties. Use `any-props?` option to override"
                      (m/explain SupportedProperties props))))))


(defprotocol GNode
  (type-def [this val-defs] "returns the type definition string for the AST type")
  (transform-children [this] "transforms ast parsed from malli to g-ast specific to GraphQL")
  (transform [this] "recursively transform ast to g-ast if type is not wrapping")
  (stringify [this] "stringify node and child nodes")
  (enclose [this strs] "uses characters to separate (and enclose) stringified children"))

(defn- dispatch-ast->g-ast [node options context]
  ;; raise exception if schemas that would result in nesting are encountered
  (check-depth! (:depth context) {:node node})
  (cond (some-> node :schema (m/properties options) ::external-type) :external-type
        (not (some? node)) :nil-node
        (:$ref node) :$ref
        (:=> node) [:type :=>]
        (:type node) [:type (:type node)]
        (:union node) :union
        (:enum node) :enum
        (:intersection node) :intersection
        (:maybe node) :maybe
        (some? (:const node)) :const
        :else [:type :any]))

;; each method calls transform
(defmulti -ast->g-ast
  #'dispatch-ast->g-ast)

(defmethod -ast->g-ast :$ref
  [{:keys [$ref] :as node} {:keys [deref-types
                                   schema-id->type-desc]
                            :as options} context]
  ;; definitions from options override local ones
  (when (or (get deref-types $ref) (not (get schema-id->type-desc $ref)))
    (-ast->g-ast
     (or (get-in node [:definitions $ref])
         (->ast (:schema node)))
     options
     (merge
      context
      (when (pos? (:depth context)) ;; Only needed for refs tests. See comment there
        {:ref-name $ref})
      {:root-name $ref
       :depth 0}))))

(defmethod -ast->g-ast :external-type [{:keys [schema]} _ _]
  (::external-type (m/properties schema)))

(def letter-args (for [n (cons " " (map str (range)))
                       c "abcdefghijklmnopqrstuvwxyz"]
                   (str c n)))


(defn ast->g-ast
  ([node options context]
   (-ast->g-ast node options context))
  ([node options]
   (ast->g-ast node options {:depth 0})))

(defn- ->g-ast-children [{:keys [options context] :as gnode} & [child-ctx]]
  (update-children gnode #(-ast->g-ast % options (or child-ctx context))))

(defn ->nullable-children [gnode]
  (update-children gnode ->nullable))

(defn- root-name [gnode]
  (get-in gnode [:context :root-name] *undefined-type-name*))

(defn- ref-name [gnode]
  (get-in gnode [:context :ref-name]))

(defn- gql-type [gnode]
  (-> gnode -|gnode-props :gql-type))

(defn- type-name [gnode]
  (-> (gql-type gnode)
      gql-type-attributes
      :type-name))

(defn- type-extend [gnode]
  (when (-> gnode -|gnode-props :extend) "extend"))

(defn- inc-depth [gnode]
  (update-in gnode [:context :depth]
             (fnil inc 0)))

(defn- reset-context [gnode]
  (update gnode :context #(dissoc % :nullable)))

(defn- stringify-def-scalar [gnode]
  (when-some [def-val (:default (-|gnode-props gnode))]
    (case (get-in gnode [:node :type])
      :string (s-lib/format "\"%s\"" def-val)
      (str def-val))))

(defprotocol GWrappingType)

(defn- root-def? [gnode]
  (boolean (and
            (not (get-in gnode [:node :primitive-name]))
            (not (get-in gnode [:context :ref-name]))
            (= (-|gnode-depth gnode) 1)
            (not (satisfies? GWrappingType gnode)))))

(defn- suffix-! [gnode string]
  (str string (when-not (or
                         ;; suffix only if at root and not dereffed
                         (root-def? gnode)
                         (get-in gnode [:context :non-nullable])
                         (nullable? gnode)) \!)))

;; 3.11. List
(defrecord GList [options context node children]
  GWrappingType
  GNode
  (transform-children [this] (->g-ast-children this))
  (transform [this] (-> this reset-context transform-children))
  (enclose [this strs] (s-lib/format "[%s]" strs))
  (stringify [this] (suffix-! this (enclose this (stringify (-|single-val children))))))

;; 3.12 Non-Null (note! uses the inverse `:maybe` in malli)
(defrecord GNullable [options context node children]
  GWrappingType
  GNode
  (transform-children [this] (-> this ->g-ast-children ->nullable-children))
  (transform [this] (-> this ->opt transform-children))
  (stringify [_] (stringify (-|single-val children))))

;; 3.5 Scalars
(defrecord GScalar [options context node children]
  GNode
  (type-def [{{:keys [root-name]} :context :as this} val-defs]
    (if (root-def? this)
      (s-lib/format "scalar %s" (or root-name *undefined-type-name*))
      val-defs))
  (transform [this] (-> this reset-context))
  (stringify [{{:keys [primitive-name]} :node :as this}]
    (str/join *inline-separator*
              (concat [(suffix-! this ; non-nullable suffix goes before default arguments
                                 (type-def this (or (ref-name this) primitive-name)))]
                      (when-let [def-str (stringify-def-scalar this)]
                        [\= def-str])))))

;; 3.8 Unions
(defrecord GUnion [options context node children]
  GNode
  (type-def [this val-defs]
    (s-lib/format "union %s = %s" (root-name this) val-defs))
  (transform-children [{:keys [context] :as this}] (->g-ast-children this (assoc context :non-nullable true)))
  (transform [this] (-> this inc-depth transform-children))
  (enclose [_ strs] (str/join "|" strs))
  (stringify [this] (suffix-! this (or (ref-name this) (type-def this (enclose this (map stringify children)))))))

;; 3.9 Enums
(defrecord GEnum [options context node children]
  GNode
  (type-def [this val-defs]
    (str/join *inline-separator*
              ["enum"
               (get-in this [:context :root-name] *undefined-type-name*)
               val-defs]))
  (transform-children [{:keys [context] :as this}] (->g-ast-children this (assoc context :non-nullable true)))
  (transform [this] (-> this inc-depth transform-children))
  (enclose [_ strs] (s-lib/format "{%s}" (str/join *inline-separator* strs)))
  (stringify [this] (suffix-! this (or (ref-name this) (type-def this (enclose this (map stringify children)))))))

;; 3.6.1 Field Arguments
(defrecord GArgumentsDefinition [options context node children]
  GNode
  (enclose [this content] (s-lib/format "(%s)" (str/join *inline-separator* content)))
  (transform-children [this] (update this :children (partial map transform)))
  (transform [this] (-> this transform-children))
  (stringify [{:keys [children] :as this}] (enclose this (map stringify children))))

(defn- ->fn-entry [{:keys [children] :as gnode}]
  (let [[_k v] children]
    (cond-> gnode (-> v :kind #{:fn}) (-> (assoc-in [:context :kind] :fn-entry)))))

(defrecord -GEntry [options context node children]
  GNode
  (transform-children [this] (->g-ast-children this))
  (transform [this] (-> this transform-children ->fn-entry))
  (stringify [{:keys [children]}] (let [[k v] (-|pair-vals children)
                                        [k-str v-str] (map stringify [k v])]
                                    (case (get-in v [:node :kind])
                                      :fn (str k-str v-str)
                                      (str k-str *key-val-separator* v-str)))))

(defn- kv->entry-g-ast [options context node [k v]]
  (-GEntry. options context node [(->ast [:= k]) v]))

;; 3.6 Object (3.10 Input Object is similar but more restrictive about entries)
(defrecord GObject [options context node children]
  GNode
  (type-def [this val-defs]
    (str/join *inline-separator*
              (keep identity
                    [(type-extend this)
                     (type-name this)
                     (when-not (= (gql-type this) ::gql-schema)
                       (root-name this))
                     val-defs])))
  (enclose [this strs] (s-lib/format "{%s}" strs))
  (transform [this] (-> this inc-depth transform-children))
  (stringify [{:keys [children] :as this}]
    (suffix-! this (or (ref-name this)
                       (type-def this (enclose this (str/join *line-separator* (map stringify children)))))))
  (transform-children [{:keys [options context] :as this}]
    (update this :children #(->> %
                                 (map (partial kv->entry-g-ast options context {}))
                                 (map transform)))))

;; Internal representation of a constant. Used for enums, function keywords
(defrecord -GConst [options context node children]
  GNode
  (transform [this] this)
  (stringify [{{:keys [const-val]} :node :as this}]
    (or (ref-name this)
        (cond
          (keyword? const-val) (name const-val)
          (string? const-val) const-val
          :else (not-implemented! "Only keyword or string constants are supported")))))

(defrecord -GFunction [options context node children]
  GNode
  (transform [this] (transform-children this))
  (transform-children [{[args ret] :children :as this}]
    (assoc this :children [(->> args
                                (map (partial kv->entry-g-ast options (merge context {:kind :fn-pars}) node))
                                (GArgumentsDefinition. options context {})
                                transform)
                           (-ast->g-ast ret options context)]))
  (stringify [{:keys [children]}] (apply s-lib/format "%s:%s" (map stringify (-|pair-vals children)))))

;; 3.12 (Non-)Nullable (malli asumes non-null by default)
(defmethod -ast->g-ast :maybe [node options context] (transform (GNullable. options context node [(first (:maybe node))])))

;; 3.11 List
(defmethod -ast->g-ast [:type :array] [node options context] (transform (GList. options context node [(:items node)])))

;; 3.5 Scalars
(defn- scalar-node [node options context builtin-name]
  (transform (GScalar.
              options
              context
              (assoc node :primitive-name builtin-name
                     :default-value (:default (-|gnode-props node)))
              nil)))

(defmethod -ast->g-ast [:type :ID] [node options context] (scalar-node node options context "ID"))
(defmethod -ast->g-ast [:type :float] [node options context] (scalar-node node options context "Float"))
(defmethod -ast->g-ast [:type :integer] [node options context] (scalar-node node options context "Int"))
(defmethod -ast->g-ast [:type :string] [node options context] (scalar-node node options context "String"))
(defmethod -ast->g-ast [:type :boolean] [node options context] (scalar-node node options context "Boolean"))
(defmethod -ast->g-ast :enum [node options context] (transform (GEnum. options context node (:enum node))))
(defmethod -ast->g-ast :union [node options context] (transform (GUnion. options context node (:union node))))
(defmethod -ast->g-ast :const [node options context] (transform (-GConst. options context (assoc node :const-val (:const node)) nil)))

(defmethod -ast->g-ast [:type :object] [{:keys [properties] :as node} options context]
  (transform (GObject. options context node properties)))

;; 3.6.1 Field Arguments
(defmethod -ast->g-ast [:type :=>] [{:keys [args ret] :as node}
                                    {:keys [args-names] :as options}
                                    context]
  (let [args-type (get args :type)
        args-items (get args :items)
        args-names (or args-names
                       (case args-type
                         :catn (map (comp name first) args-items)
                         :cat (take (count args) letter-args)))
        args (if (= args-type :catn)
               (map (fn [[_ a]] a) args-items)
               args-items)]
    (transform (-GFunction. options context {:kind :fn}
                            [(map list args-names args)
                             ret]))))


;;;
;;; Functions for working with malli schemas
;;;

(defn add-default-properties
  "Takes a malli vector schema and adds type-specific properties"
  [vec-schema]
  (letfn [(set-type-prop [[t & [s & rst :as tail]] t-name]
                          ;; check if the schema vector has properties
            (if (map? s)
              (into [t (assoc s :gql-type t-name)] rst)
              (into [t {:gql-type t-name}] tail)))]
    (postwalk #(if (vector? %)
                 (cond-> %
                   (= :Object (first %)) (set-type-prop ::object)
                   (= :InputObject (first %)) (set-type-prop ::input-object)
                   (= :Schema (first %)) (set-type-prop ::gql-schema))
                 %) vec-schema)))

(defn -malli->graphql
  [schema options & [context]]
  (-> schema
      ->ast
      (ast->g-ast options (merge {:depth 0} context))
      stringify))

(defn malli->graphql
  "`malli->graphql` takes a malli vector schema and converts it to a GraphQL string.
   Schemas can be named by passing to `context` {... :root-name <name> ...}"
  [vec-schema & [options context :as args]]
  (apply -malli->graphql (add-default-properties vec-schema) args))

(defn- -registry-kvs->graphql [registry-kvs options]
  (let [registry-m (into (hash-map) registry-kvs)
        context-registry (mr/composite-registry
                          m/default-registry
                          registry-m
                          (:registry options))]
    (->> registry-kvs
         (map (fn [[root-name schema]]
                (-> (m/schema schema {:registry context-registry})
                    (try (catch #?(:clj Exception :cljs js/Error) _
                           (throw (ex-info "Failed to parse to schema" {:schema schema}))))
                    (malli->graphql options {:root-name root-name}))))
         (str/join *line-separator*))))

(defn registry-vals->graphql
  "Takes a map-like flat vector (of registry-keys and values) (e.g. [\"SchemaName\" [:enum :b :c]])
  and converts it to GraphQL. The cljs supports prettifying using a truthy `:clean?` as an `options`"
  [registry-vec options]
  (let [#?@(:clj [println-stderr #(binding [*out* *err*] (println %))])
        registry-m (sequence (comp (partition-all 2)
                                   (map (fn [[k v]] [k (add-default-properties v)])))
                             registry-vec)]
    (cond-> (-registry-kvs->graphql registry-m options)
      #?@(:clj [(when (:clean? options)
                  (println-stderr "WARNING: `clean?` option only supported in cljs")) identity]
          :cljs [(:clean? options) clean]))))

(defn read-edn
  "Load malli registry edn from an io/reader source."
  [source]
  #?(:cljs (read-string (read-file source))
     :clj (try
            (with-open [r (io/reader source)]
              (edn/read (PushbackReader. r)))
            (catch java.io.IOException e
              (throw (ex-info (s-lib/format "Couldn't open '%s': %s\n" source (.getMessage e)) {})))
            (catch RuntimeException e
              (throw (ex-info (s-lib/format "Error parsing edn file '%s': %s\n" source (.getMessage e)) {}))))))

(defn read-concat-registry-vec
  "Load malli registry edn from an io/reader source and convert to graphql."
  [sources]
  (reduce into (map read-edn sources)))

(defn malli-registry-edn->graphql
  "Load malli registry edn from an io/reader source and convert to graphql.
  Provide registry sources as a sequence, where each source gets output using the others as registries.
  See `registry-vals->graphql` for options"
  ([sources options]
   (for [batch (take (count sources) (iterate next (cycle sources)))
         :let [[primary-source & secondary-sources] (take (count sources) batch)]]
     (malli-registry-edn->graphql primary-source secondary-sources options)))
  ([primary-source secondary-sources options]
   (let [full-registry (apply hash-map (read-concat-registry-vec (conj secondary-sources primary-source)))
         primary-registry (read-edn primary-source)]
     (registry-vals->graphql primary-registry (assoc options :registry full-registry)))))

#?(:clj (def write-file clojure.core/spit)
   :cljs (def write-file utils/write-file))

(defn convert-malli-registry-edn
  "Load malli registry edn from an io/reader source and write converted graphql to `out-file`."
  [source out-path options]
  (write-file out-path (malli-registry-edn->graphql source options)))

(defn ->standard-malli [vec-schema & [options]]
  (->> vec-schema
       add-default-properties
       (postwalk #(graphql-type->malli-schema % options))))

(defn standardize-registry-edn [source-edn out-path]
  (->> source-edn read-edn ->standard-malli pp/pprint with-out-str  (write-file out-path)))

;; ;;;
;; ;;; Usage as a node cli program
;; ;;;

;; #?(:cljs (nodejs/enable-util-print!)

;;    (defn convert-file-cli
;;      "Entrypoint to the library as a cli program"
;;      [[source-file dest-file clean?]]
;;      (let
;;       (println source-file dest-file clean?)
;;        (convert-malli-registry-edn source-file dest-file (if clean?
;;                                                            {:clean? true}
;;                                                            {}))))

;;    (set! *main-cli-fn* convert-file-cli))
