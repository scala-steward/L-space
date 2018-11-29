---
layout: docs
title: Design
position: 1
---

# Design
Moving towards Linked (Open) Data can be challenging w.r.t the technical requirements (graph computing) and semantics. 
L-space's data model attempts to create a data-space with less restrictions, more data and advanced traversing constructs. 

## Data model
The data-model for L-space must be non-restrictive to allow for patterns which where not accounted for: Open World Assumption (OWA).  
Let's start with a Resource. A resource can have edges (any direction). Then there are three other types which all extends Resource:
* **Value**: a value is a vertex which holds a literal-value and its type is described by a data-type
* **Edge**: a edge is a link from a resource to another resource and its type is described by a property
* **Node**: a node is a vertex and can be labeled by zero or more ontolgies
These are the core elements in a graph

And then there is the concept of an ontology, property and data-type:
* **Ontology**: a named collection of properties. The properties within an ontology can direct users and developers to what relations are meaningful or just predefined for the ontologies in scope. This, of course, never restricts from creating edges with other property-types. This can lead to unexpected of even illogical relations. So no need to reject unknown structures, just scrape all knowledge and let the analysis reason about the content of the graph OWA!
* **Property**: a named relation with a range containing of zero or more guiding ontologies and/or datatypes. This range, like an ontology, never restrict the graph from creating edges to resources outside this range.
* **DataType**: a scala data type or advanced types
  * Scala datatypes:
    * Numeric: Int, Double, Long
    * String
    * Temporal (java.time): Instant, LocalDate, LocalTime
    * Geo: Geometry (work-in-progress, open-for-suggestions)
    * Boolean
  * [Squants DSL (WIP)](https://github.com/typelevel/squants)
  
## Graph model
A graph exists of a data-space, name-space and index-space. 

## Rationales
This project is inspired by the [Apache Tinkerpop](http://tinkerpop.apache.org/) graph computing framework 
and it's query language Gremlin. I was experiencing too much restrictions with Gremlin and a lack of type-safety. 
I was in need of polymorphic-types, -ordering and -indexing. 
I also think there should not be any limitation on what is linkable 
(e.g. the range of a Property can link to a both a [DataType](http://schema.org/DataType) and 
[Thing](http://schema.org/Thing)). A subject/predicate/object can be a node/relation/node, a 
node/relation/value, a relation/relation/value, a value/relation/value, a value/relation/node or a 
relation/relation/node...OWA! All of which is not supported in Gremlin.

## Approach
Because I like the semantic of Gremlin I took it as a reference for Librarian. Librarian has its differences, not all Librarian steps share all of their characteristics their Gremlin counterparts. For example, Librarian does not have a properties-step because in- and out-step traverse all of the resource types (Value, Edge, Node). So w.r.t. Linked Data: take for example the property [schema.org/address](https://schema.org/address), the range is [schema.org/text](https://schema.org/Text) and [schema.org/PostalAddress](https://schema.org/PostalAddress). So the property schema.org/address could lead to either a text-value or a postaladdress-node. Hence the Librarian traverses them equally and the result can be a mix of text-values and address-nodes (and of course other types because L-space has no restrictions!). Don't be frightened, there is also a step to filter only on certain types or assert the values to a certain predicate within in-/out-step. Furthermore it is still very much a WIP. 
I deliberately (for now) left out SPARQL and GraphQL because the lack various advanced constructs for traversing a graph. 
All L-space graph traversal steps are defined within the graph-data-model, hence it is serializable to Json-LD and parsed back from.
