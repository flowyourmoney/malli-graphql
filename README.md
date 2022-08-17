# malli-graphql

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.flowyourmoney/malli-graphql.svg)](https://clojars.org/org.clojars.flowyourmoney/malli-graphql)

*State: early testing!*

A library for generating [GraphQL](https://graphql.org/) schemas from [Malli schemas](https://github.com/metosin/malli)

Based on the [GraphQL specification](https://spec.graphql.org/October2021/) October 2021 Edition

## Usage
### Writing compatible malli schemas
Aliases for supported GraphQL types (see Features) are provided for convenience.

They point to `:map` `:or` `:enum` `:vector` `:int` `:double` `:string` `:boolean` malli schemas.

In order to differentiate between *Objects*, *Input Objects*, and *Schemas*, schema properties are used (see tests for examples).

### Converting schemas
1. Add the dependency according to your dependency management toolchain from [Clojars](https://clojars.org/org.clojars.flowyourmoney/malli-graphql).

2. Import the library either as a node module or as a Clojure library

3. The functions below are used to generate GraphQL either by reading from malli schemas from a file or from a string.

| function name              | conversion description                                                      | arguments                                 |
|----------------------------|-----------------------------------------------------------------------------|-------------------------------------------|
| malli->graphql             | malli schema to graphql string (name as options {:root-name \<schema name\>}) | [vec-schema & [options context :as args]] |
| registry-vals->graphql     | map-like registry vector of names (string) & vector schemas to string       | [registry-vec options]                    |
| convert-malli-registry-edn | file containing registry-vals (as above) to graphql file                    | [source out-path options]                 |

`registry-edn` refers to a map-like vector of keys and values where keys contain the schema name and values contain malli vector schemas.
for example
```
["SchemaName" [:enum :b :c]]
```


### Formatting using GraphQL.js

When using the library as a CLJS library, the `:clean?` value of `options` in  `registry-vals->graphql` can be set to provide formatting to the resulting GraphQL. This is done by parsing and printing the GraphQL schema using the official GraphQL.js library.

## Features
Currently, basic (non-optional) features of `ID`, `Boolean`, `InputObject`, `Float`, `Int`, `Union`, `Schema`, `List`, `Enum`, `Object`, `String` are implemented.

### Limitations
Objects and InputObjects are treated similarly except for their name.
Outputted schemas are not checked for validity (see Formatting using GraphQL.js as an exception).
Descriptions, Directives, contentless type declarations, as well as many other optional features are not implemented.

## Acknowledgements
This library is based on [malli-ts](https://github.com/flowyourmoney/malli-ts)
