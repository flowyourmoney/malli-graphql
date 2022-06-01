(ns malli-graphql.tests.core-test
  (:require #?(:clj [clojure.test :refer [deftest testing is use-fixtures]]
               :cljs [cljs.test :refer-macros [deftest testing is use-fixtures]])
            #?(:cljs [malli-graphql.utils :refer [clean]])
            [malli-graphql.ast :refer [->ast]]
            [malli-graphql.core :as gql :refer [ast->g-ast set-gql-registry! nullable?
                                                malli->graphql read-malli-registry-edn
                                                registry-vals->graphql stringify]]))

#?(:clj (use-fixtures :each (fn [f] (set-gql-registry!) (f)))
   :cljs (use-fixtures :each {:before (fn [] (set-gql-registry!))}))

(def ^:private gast-tree-seq (partial tree-seq :children :children))

;; To make this work no undefined types should exist (output full registry and no consts)
(comment (def = #?(:clj clojure.core/=
                   :cljs (fn [s1 s2]
                           (is (cljs.core/= (clean s1) (clean s2)))))))

(deftest registry
  (testing "initialize registry atom with graphql builtin type definitions"
    (testing "default-registry names resolve to explicitly set names in definition"
      (is (= "union Squats = Int|Float"  (-> ["Squats" [:or :Int :Float]]
                                             (registry-vals->graphql {})))))
    (testing "references to incorporated registry entries resolve to reference name"
      (is (= "union Order = Apple|Banana" (-> [:schema {:registry {"Apple" [:= :A]
                                                                   "Banana" [:= :B]}}
                                               [:or "Apple" "Banana"]]
                                              (malli->graphql {} {:root-name "Order"})))))
    (testing "convert schemas in registry to graphql"
      (is (= "union Order = Goat|String" (malli->graphql [:schema {:registry
                                                                   {"Goat" :Int}}
                                                          [:or "Goat" :String]] {} {:root-name "Order"}))))))

(deftest context
  (testing "Derefs from the registry don't contribute to depth"
    (let [chained-ref-gast (-> [:schema {:registry
                                         {"Apple" [:= :A]
                                          "Banana" [:map [:a "Apple"]]
                                          "Order" [:or "Apple" "Banana"]}}
                                "Order"]
                               ->ast
                               (ast->g-ast {}))]
      (is (every? (comp #{0 1} :depth :context) (gast-tree-seq chained-ref-gast)))))
  (testing "all gast nodes have depth in their context"
    (let [chained-ref-gast (-> [:schema {:registry
                                         {"Apple" [:= :A]
                                          "Banana" [:map [:a "Apple"]]
                                          "Order" [:or "Apple" "Banana"]}}
                                "Order"]
                               ->ast
                               (ast->g-ast {}))]
      (is (every? (comp :depth :context) (gast-tree-seq chained-ref-gast))))))

(deftest object
  (testing "Object"
    (testing "values can be nullable"
      (let [gast (-> ["Book" [:Object
                              [:hardcover  :Boolean]
                              [:illustrated [:maybe :Boolean]]]]
                     (registry-vals->graphql {}))]
        (is (->> (gast-tree-seq gast)
                 (map (comp #(if % 1 0) :nullable :context))
                 (reduce + 0)) 1)))
    (testing "internal entry nodes don't contribute to depth"
      (let [gast (ast->g-ast
                  (->ast [:map
                          [:hardcover :Boolean]
                          [:illustrated [:maybe :Boolean]]]) {})
            max-depth (->> (gast-tree-seq gast)
                           (map (comp :depth :context))
                           (apply max))]
        (is (= max-depth 1))))
    (testing "can have arguments (function values)"
      (let [gast (-> [:schema {:registry {"Monument" [:map [:isOpen [:=> [:catn [:time :string]] :Boolean]]]}} "Monument"]
                     ->ast
                     (ast->g-ast {}))]))
    (testing "serialization simple test"
      (let [schema-str (->
                        [:map {:gql-type ::gql/object}
                         [:hardcover :Boolean]
                         [:illustrated [:maybe :Boolean]]]
                        (malli->graphql {} {:root-name "Book"}))]
        (is (= "type Book {hardcover:Boolean! illustrated:Boolean}" schema-str)))))
  (testing "Schema"
    (testing "for schemas the name isn't displayed"
      (let [schema-str (-> ["DontDisplayMe" [:map {:gql-type ::gql/gql-schema}
                                             [:query [:maybe [:= :Query]]]
                                             [:mutation [:maybe [:= :Mutation]]]]]
                           (registry-vals->graphql {}))]
        (is (= "schema {query:Query mutation:Mutation}" schema-str))))))

(deftest enum
  (testing "simple enum types"
    (let [m-schema [:enum :a :b]]
      (is (= "enum Alphabet {a b}" (malli->graphql m-schema {} {:root-name "Alphabet"}))))))

(deftest nullable-nodes
  (testing "nullable node applies to child node"
    (is (= "type Thought {deepz:[[Int]]!}"
           (-> [:map {:gql-type ::gql/object} [:deepz [:vector [:maybe [:vector [:maybe :Int]]]]]]
               (malli->graphql {} {:root-name "Thought"})))))
  (testing "nullability is not propagated"
    (let [gast (ast->g-ast (->ast [:vector [:maybe [:vector :Int]]]) {})
          num-optionals (->> (gast-tree-seq gast)
                             (map (comp #(if (nullable? %) 1 0)))
                             (reduce + 0))]
      (is (= num-optionals 1))))
  (testing "nullable nodes don't contribute to depth"
    (let [gast (ast->g-ast (->ast [:vector [:maybe [:vector [:maybe :Int]]]]) {})
          max-depth (->> (gast-tree-seq gast)
                         (map (comp :depth :context))
                         (apply max))]
      (is (= max-depth 0))))
  (testing "in non-optional contexts ! are omitted"
    (is (= "union Order = Banana" (-> [:schema {:registry {"Banana" :string}}
                                       [:Union "Banana"]]
                                      (malli->graphql {} {:root-name "Order"})))))
  (testing "in optional contexts ! are shown"
    (testing "for references")))

(deftest refs

  (testing "references have non-nullable suffixes"
    (testing "when ref child context is non-nullable"
       ;; For some reason (specific to malli?) schema properties are not read when they are wrapped with :schema so we give a plain ref-name instead
      (let [m-schema [:schema {:registry
                               {"Seed" [:Enum :b :c]
                                "Banana" [:Object [:seed [:maybe "Seed"]]]}}
                      "Banana"]]
        (is (= "type Banana {seed:Seed}"
               (-> m-schema
                   (malli->graphql {} {:root-name "Banana"})))))))
  (testing "references have non-nullable suffixes"
    (let [m-schema [:schema {:registry
                             {"Peel" [:map [:color :string]]
                              "Banana" [:Object [:container "Peel"]]}}
                    "Banana"]]
      (is (= "type Banana {container:Peel!}"
             (-> m-schema
                 (malli->graphql {} {:root-name "Banana"}))))))
  (testing "reference to nonexisting schema"
    (let [m-schema [:schema {:registry
                             {"Steps" :Int}}
                    "Leaps"]]
      (try (->ast m-schema)
           (is false)
           (catch #?(:cljs js/Error :clj Exception) _
             (is true))))))

(deftest function-nodes
  (testing "serialization of plain function"
    (let [schema-str (stringify (ast->g-ast (->ast [:map {:gql-type ::gql/object} [:fn-name [:=> [:catn [:x :String] [:y :Int]] :Int]]]) {}))]
      (is (= "type UNDEFINED {fn-name(x:String! y:Int!):Int!}" schema-str))))
  (testing "serialization of function with default argument"
    (let [schema-str
          (-> ["ImportantSchema"
               [:Object
                [:fn-name [:=> [:catn [:x [:maybe [:String
                                                   {:default "meaning of life"}]]]]
                           :Int]]]]
              (registry-vals->graphql {}))]
      (is (= "type ImportantSchema {fn-name(x:String = \"meaning of life\"):Int!}" schema-str)))))

#?(:cljs (deftest clean-graphql
           (testing "use js graphql library to pretty print graphql"
             (let [reg-vals ["Book" [:Object
                                     [:hardcover  :Boolean]
                                     [:illustrated [:maybe :Boolean]]]]]
               (is (= (-> reg-vals (registry-vals->graphql {:clean? true}))
                      "type Book {
  hardcover: Boolean!
  illustrated: Boolean
}"))))))

(deftest builtin-types-with-defaults
  (testing "using shorthand alias is equivalent to use with properties"
    (let [s1-ast (malli->graphql [:map {:gql-type ::gql/object} [:a :String]] {})
          s2-ast (malli->graphql [:Object [:a :String]] {})]
      (is (= s1-ast s2-ast)))))

(deftest read-edn
  (testing "read malli schemas registry from edn file"
    (is (= (registry-vals->graphql
            ["a" [:Object [:x :String]]
             "b" [:or "a" :Int]
             "c" [:or "a" "b" [:= :c]]] {})
           (read-malli-registry-edn "tests/resources/test_registry.edn" {})))))

