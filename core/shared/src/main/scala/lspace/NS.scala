package lspace

import lspace.types.string.Prefix

object NS {
  object vocab {
    val foaf   = Prefix("http://xmlns.com/foaf/0.1/")
    val dc     = Prefix("http://purl.org/dc/terms/")
    val rdf    = Prefix("https://www.w3.org/1999/02/22-rdf-syntax-ns#")
    val owl    = Prefix("https://www.w3.org/2002/07/owl#")
    val skos   = Prefix("https://www.w3.org/2009/08/skos-reference/skos.html#")
    val dcam   = Prefix("http://dublincore.org/2012/06/14/dcam#")
    val rdfs   = Prefix("https://www.w3.org/2000/01/rdf-schema#")
    val schema = Prefix("https://schema.org/")
    val sioc   = Prefix("http://rdfs.org/sioc/spec/#")
  }

  object types {
    val `@literal` = "@literal"
    val `@string`  = "@string"
    val `@iri`     = "@iri"
    val `@number`  = "@number"
    val `@int`     = "@int"
    val `@double`  = "@double"
    val `@long`    = "@long"
    val `@boolean` = "@boolean"
    //    val enum = "enum"
    val `@temporal`         = "@temporal"
    val `@date`             = "@date"
    val `@time`             = "@time"
    val `@quantity`         = "@quantity"
    val `@duration`         = "@duration"
    val `@datetime`         = "@datetime"
    val `@localdatetime`    = "@localdatetime"
    val `@epoch`            = "@epoch"
    val `@color`            = "@color"
    val `@geo`              = "@geo"
    val `@geojson`          = "@geojson"
    val `@geopoint`         = "@geopoint"
    val `@geomultipoint`    = "@geomultipoint"
    val `@geoline`          = "@geoline"
    val `@geopolygon`       = "@geopolygon"
    val `@geomultipolygon`  = "@geomultipolygon"
    val `@geomultigeometry` = "@geomultigeometry"
    val `@geomultiline`     = "@geomultiline"
    val geoshape            = NS.vocab.schema + "GeoShape"
    val geocircle           = NS.vocab.schema + "GeoCircle"
    val address             = NS.vocab.schema + "PostalAddress"
    //    val literals = Seq(string, int, double, long, boolean, /*enum, */date, time, datetime, epochtime,
    //      color, geojson, geoshape, geocircle, address, URL)

    val `@context`           = "@context"
    val `@id`                = "@id"
    val `@ids`               = "@ids"
    val `@value`             = "@value"
    val `@from`              = "@from"
    val `@to`                = "@to"
    val `@pvalue`            = "p@value"
    val `@name`              = "@name"
    val `@language`          = "@language"
    val `@type`              = "@type"
    val `@container`         = "@container"
    val `@structured`        = "@structured"
    val `@collection`        = "@collection"
    val `@map`               = "@map"
    val `@tuple`             = "@tuple"
    val `@list`              = "@list"
    val `@set`               = "@set"
    val `@listset`           = "@listset"
    val `@vector`            = "@vector"
    val `@single`            = "@single"
    val `@entry`             = "@entry"
    val `@reverse`           = "@reverse"
    val `@index`             = "@index"
    val `@base`              = "@base"
    val `@vocab`             = "@vocab"
    val `@graph`             = "@graph"
    val `@nest`              = "@nest"
    val `@prefix`            = "@prefix"
    val `@version`           = "@version"
    val `@label`             = "@label"
    val `@comment`           = "@comment"
    val `@resource`          = "@resource"
    val `@class`             = "@class"
    val `@property`          = "@property"
    val `@properties`        = "@properties"
    val `@extends`           = "@extends"
    val `@datatype`          = "@datatype"
    val rdfsClass            = NS.vocab.rdfs + "Class"
    val rdfsSubClassOf       = NS.vocab.rdfs + "subClassOf"
    val rdfsSubPropertyOf    = NS.vocab.rdfs + "subPropertyOf"
    val rdfsDomain           = NS.vocab.rdfs + "domain"
    val rdfsIsDefinedBy      = NS.vocab.rdfs + "isDefinedBy"
    val rdfsLabel            = NS.vocab.rdfs + "label"
    val rdfsComment          = NS.vocab.rdfs + "comment"
    val rdfProperty          = NS.vocab.rdf + "Property"
    val schemaClass          = NS.vocab.schema + "Class"
    val schemaDomainIncludes = NS.vocab.schema + "domainIncludes"
    val schemaInverseOf      = NS.vocab.schema + "inverseOf"
    val schemaRange          = NS.vocab.schema + "rangeIncludes"
    val schemaDataType       = NS.vocab.schema + "DataType"
    val schemaTime           = NS.vocab.schema + "Time"
    val schemaDate           = NS.vocab.schema + "Date"
    val schemaText           = NS.vocab.schema + "Text"
    val schemaNumber         = NS.vocab.schema + "Number"
    val schemaFloat          = NS.vocab.schema + "Float"
    val schemaInteger        = NS.vocab.schema + "Integer"
    val schemaDateTime       = NS.vocab.schema + "DateTime"
    val schemaBoolean        = NS.vocab.schema + "Boolean"
    val schemaURL            = NS.vocab.schema + "URL"
    val `@nodeURL`           = "@nodeURL"
    val `@edgeURL`           = "@edgeURL"
    val `@valueURL`          = "@valueURL"

    val `@range`         = "@range"
    val `@start`         = "@start"
    val `@end`           = "@end"
    val `@createdon`     = "@createdon"
    val `@deletedon`     = "@deletedon"
    val `@modifiedon`    = "@modifiedon"
    val `@transcendedon` = "@transcendedon"

    //custom
    val min = "min"
    val max = "max"

    //    val reservedWords = Seq(context, id, ids, value, language, TYPE, container, list, set, reverse, index, base,
    //      vocab, graph, nest, prefix, version, label, comment)
  }

}
