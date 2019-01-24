---
layout: docs
title: Graph guide
position: 2
---

# Graph Guide
* [What is a graph](#what-is-a-graph)
* [Resource API's](#resource-api's)
  * [Resources API](#resources-api)
    * [Resource](#resource)
  * [Nodes API](#nodes-api)
    * [Node](#node)
  * [Edges API](#edges-api)
    * [Edge](#edge)
  * [Values API](#values-api)
    * [Value](#value)
* [History](#history)
 
 ```tut:invisible
 import lspace.librarian.process.traversal.P
 import lspace.librarian.provider.mem.MemGraph
 import lspace.librarian.structure._
 import lspace.librarian.datatype.DataType.default._
 import lspace.librarian.util.SampleGraph
 ```
 
## What is a graph
A graph is a collection of nodes, edges and values, these are all resources. 
Nodes can be labelled and nodes can have edges.
Edges have a source ('from') and a destination ('to') resource which can both be either a node, edge or value. 
The edge is always labeled with a key (property) and an edge can also be the source to other edges (statements on edges).
Values have a data-value and values can have edges.

* Graphs support Librarian-traversals
* Graphs can be merged (graph ++ graph)
* Graphs support transactions (graph.transaction -> new in-memory graph with 'commit' function)

create a graph
 ```tut:book
 val graph: Graph = MemGraph("graph-doc")
 SampleGraph.loadSocial(graph)
 val labels = SampleGraph.ontologies
 val keys = SampleGraph.properties
 ```
 
## Resource API's
Graph-resources can all be retrieved by Iri (@id) or by Id. Values and edges do mostly not have an Iri (empty-string).
There are API's on each structure-level (resource, value, edge and node). 

The main methods per section:

### Resources API

hasIri
```tut:book
graph.resources.hasIri("graph-doc/place/123")
graph.resources.hasIri("graph-doc/place/123", "graph-doc/person/123")
```
hasId
```tut:book
val id = graph.resources.hasIri("graph-doc/place/123").head.id
graph.resources.hasId(id)
```
upsert
```tut:book
graph.values.create("some-literal").id
graph.values.create("some-literal").id //should be equal because values are deduplicated
graph.values.create("some-literal2").id //new value, hence new id
```
count:
```tut:book
graph.resources.count()
```

#### Resource
A resource ..
 
### Nodes API
Nodes can be: 

Retrieved by Iri(s)/Uri(s):
```tut:book
graph.resources.hasIri("graph-doc/place/123")
graph.resources.hasIri("graph-doc/place/123", "graph-doc/person/123")
```
 
Retrieved by Id(s):
```tut:book
val id = graph.resources.hasIri("graph-doc/place/123").head.id
graph.resources.hasId(id)
```

Counted:
```tut:book
graph.nodes.count()
```

#### Node
A node ..

### Edges API
Edges can be:

Counted:
```tut:book
graph.edges.count()
```

#### Edge
An edge ..

### Values API

Counted:
```tut:book
graph.values.count()
```

#### Value
A value ..
 
## History
 A graph can have a history trait which means that resources are time aware and 
 annotated with supporting "@createdon" and "@deletedon" tags.
 