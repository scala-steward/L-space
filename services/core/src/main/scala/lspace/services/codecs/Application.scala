package lspace.services.codecs

import shapeless.Witness
import sttp.model.MediaType
import sttp.tapir.{Codec, CodecFormat}

object Application {
//  type Json = Witness.`"application/json"`.T
//  type Json = "application/json" //scala 2.13 literal types
//  type JsonLD = Witness.`"application/ld+json"`.T
//  type JsonLD = "application/ld+json" //scala 2.13 literal types
//  type SPARQL = Witness.`"application/sparql-query"`.T
//  type SPARQL = "application/sparql-query" //scala 2.13 literal types
//  type GraphQL = Witness.`"application/graphql"`.T
//  type GraphQL = "application/graphql" //scala 2.13 literal types
  type JsonCodec[T] = Codec[String, T, Json]
  case class Json() extends CodecFormat {
    override val mediaType: MediaType = MediaType("application", "json")
  }
  type JsonLDCodec[T] = Codec[String, T, JsonLD]
  case class JsonLD() extends CodecFormat {
    override val mediaType: MediaType = MediaType("application", "ld+json")
  }
  type SPARQLCodec[T] = Codec[String, T, SPARQL]
  case class SPARQL() extends CodecFormat {
    override val mediaType: MediaType = MediaType("application", "sparql-query")
  }
  type GraphQLCodec[T] = Codec[String, T, GraphQL]
  case class GraphQL() extends CodecFormat {
    override val mediaType: MediaType = MediaType("application", "graphql")
  }
}
