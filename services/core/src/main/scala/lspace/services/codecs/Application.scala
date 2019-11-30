package lspace.services.codecs

import shapeless.Witness

object Application {
  type Json = Witness.`"application/json"`.T
//  type Json = "application/json" //scala 2.13 literal types
  type JsonLD = Witness.`"application/ld+json"`.T
//  type JsonLD = "application/ld+json" //scala 2.13 literal types
  type SPARQL = Witness.`"application/sparql-query"`.T
//  type SPARQL = "application/sparql-query" //scala 2.13 literal types
  type GraphQL = Witness.`"application/graphql"`.T
//  type GraphQL = "application/graphql" //scala 2.13 literal types
}
