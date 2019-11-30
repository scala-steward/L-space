---
layout: docs
title: Design
position: 1
---

# Design
Moving towards Linked (Open) Data can be challenging w.r.t the technical requirements (graph computing) and semantics: 
How to model the data? What about meta-data (properties on properties)? 
The L-space data model attempts to create a data-space with less restrictions, more data and advanced traversing constructs. 

## Data model
The data-model for L-space allows for statements (edges) between any two resources. The different resources in a graph are:
* **Value**: a value is a vertex which holds a literal-value or structured-value, it is labeled with the corresponding data-type.
* **Edge**: an edge is a link from a resource to another resource, it is directed and it is labeled with a property. 
* **Node**: a node is a vertex and can be labeled by zero or more ontologies.

The corresponding label-types are:
* **Ontology**: is a label to a node, provides information on what the node represents and 
what are common properties to a node with this label. 
The properties within an ontology can direct users and developers to what relations are meaningful.
L-space, of course, never restricts from creating edges with other (unknown) property-types. 
This could lead to unexpected of even illogical statements. The fact that properties are not known upfront 
does not mean they cannot exist (that which is not known to be true is simply unknown - Open World Assumption).
So L-space does never reject unknown constructs, 
just scrape all knowledge, put it in a graph and let the analysis reason about the content of the graph OWA!
* **Property**: is a label to an edge, provides information on what the edge represents and 
what are common (meta-)properties to an edge with this label.
A label also provides zero or more label-types as an indication on what type of resource the edge should direct to. 
This range, like an ontology, never restrict from creating edges to resources outside of this range.
* **DataType**: is a label to a value and provides information on what the value represents. 
Supported types are:
  * Data
    * Literal
      * Numeric: Int, Double, Long
      * String
      * Boolean
    * Temporal
      * (java.time): Instant, LocalDateTime, LocalDate, LocalTime
    * Structured
      * Geo: L-space geometric types (work-in-progress)
      * Collection
        * List, Vector, Set, Map, ListSet
      * Tuple(N)
      * [Squants DSL (WIP)](https://github.com/typelevel/squants)

*A note on collection types:*  
A collection can contain any type of supported data. Collections can be nested and they can contain resources.
L-space brings nested collections to json-ld. A collection-type can be represented by a parsable, non-resolvable iri. 
E.g. a list of lists of any ```@list(@list)```, a list of integers ```list(@int)```, a list of list of integers ```@list(@list(@int))```, 
a map of stings vs list of dates ```@map(@string)(@list(@date))```, a list of persons ```@list(https://schema.org/Person``` or assumed 
there is 'schema' prefix ```@list(schema:Person)```, a polymorphic list of persons or organizations ```@list(schema:Person+schema:Organization)``` etc.

## Graph model
A graph exists of a data-space, name-space and index-space. 

* **Data Space**: 
The data space is where the real data is store.
* **Name Space**: 
The name space is where present label-types are stored (ontologies, properties and datatypes).
* **Index Space**: 
The index space is where present resource-patterns are stored. This is used to limit the solution space. 

## Rationales
This project is inspired by the [Apache Tinkerpop](http://tinkerpop.apache.org/) graph computing framework 
and it's query language Gremlin. I was experiencing too much restrictions with Gremlin (providers) and a lack of type-safety. 
I was in need of polymorphic-property-types, path/pattern-indexing and advanced namespaces w.r.t label-types.

## Approach
Because I like the semantics of Gremlin I took it as a reference for Librarian. Librarian has its differences, 
not all Librarian steps share all of their characteristics their Gremlin counterparts. For example, 
Librarian does not have a properties-step because in- and out-step traverse all of the resource types (Value, Edge, Node). 
So w.r.t. Linked Data: take for example the property [schema.org/address](https://schema.org/address), 
the range is [schema.org/text](https://schema.org/Text) and [schema.org/PostalAddress](https://schema.org/PostalAddress). 
So the property schema.org/address could lead to either a text-value or a postaladdress-node. 
Hence the Librarian traverses them equally and the result can be a mix of text-values 
and address-nodes (and of course other types because L-space has no restrictions!). 
Don't be frightened, there is also a step to filter only on certain types or assert the values to a 
certain predicate within in-/out-step. Furthermore it is still very much a WIP. 
I deliberately (for now) left out SPARQL and GraphQL because the lack various advanced constructs for traversing a graph. 
All L-space graph traversal steps are defined within the graph-data-model, 
hence it is serializable to Json-LD and parsed back from.
