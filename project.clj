(defproject org.clojars.flowyourmoney/malli-graphql "0.1.0-SNAPSHOT"
  :description "A library for generating graphql schemas from malli schemas"
  :url "https://flowyour.money/"

  :license {:name "MIT"
            :url "http://www.opensource.org/licenses/mit-license.php"}

  :dependencies
  [[metosin/malli "0.8.4"]
   [camel-snake-kebab "0.4.2"]
   [javax.xml.bind/jaxb-api "2.3.0"]]

  :source-paths
  ["src/malli_graphql"]

  :plugins [[lein-cljsbuild "1.1.8"]]

  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:output-to "out/main.js"
                                   :optimization :advanced}}]}

  :hooks [leiningen.cljsbuild]

  :repositories
  {"clojars" {:url "https://clojars.org/repo"
              :sign-releases false}}
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"])

