---
layout: docs
title: Graph guide
permalink: docs/graph-guide
---

# Graph Guide
* [What is a graph](#what-is-a-graph)
* [How to use](#how-to-use)
* [Resource API's](#resource-apis)
  * [Resources API](#resources-api)
    * [Resource](#resource)
  * [Nodes API](#nodes-api)
    * [Node](#node)
  * [Edges API](#edges-api)
    * [Edge](#edge)
  * [Values API](#values-api)
    * [Value](#value)
* [History](#history)
 
 ```scala mdoc:invisible
 import lspace._
 import lspace.Implicits.Scheduler.global
 import lspace.Implicits.SyncGuide.guide
 import lspace.provider.mem.MemGraph
 import Label.D._
 import lspace.util.SampleGraph
 ```
 
## What is a graph
A graph is a collection of nodes, unordered edges and values, these are all resources. 
Nodes can be labelled and nodes can have edges.
Edges have a source ('from') and a destination ('to') resource which can both be either a node, edge or value. 
The edge is always labeled with a key (property) and an edge can also be the source to other edges (statements on edges).
Values have a data-value and values can have edges.

* Graphs support Librarian-traversals
* Graphs can be merged (graph ++ graph)
* Graphs support transactions (graph.transaction -> new in-memory graph with 'commit' function)
* Graph support history (*never delete, only add an edge (@deletedon) to the edge .. to be tested*)

create a graph
 ```scala mdoc
 val graph: Graph = MemGraph("graph-doc")
 import scala.concurrent.duration._
 scala.concurrent.Await.ready(lspace.util.SampleGraph.loadSocial(graph).runToFuture, 5.seconds)
 val labels = SampleGraph.ontologies
 val keys = SampleGraph.properties
 ```
 
## How to use
First steps:
```scala mdoc
import lspace._ //easy access to common object (types)
import lspace.Implicits.Scheduler.global //default scheduler (execution context)
import lspace.Implicits.SyncGuide.guide //graph-engine
//import lspace.Implicits.AsyncGuide.guide //async execution of traversals
//import lspace.Implicits.RemoteGuide.guide //async remote execution of traversals (w.i.p.)
```
To create a graph (in-memory):
```scala mdoc
val graph = Graph("my-graph-url")
```
A graph always has a name, ideally this would be a url so it can be referenced.

Before continuing reading and learning, first load some sample-data:
```scala mdoc
lspace.util.SampleGraph.loadSocial(graph).runToFuture
```

Graphs have a some basis API's which allows for reading from and writing to the graph. 

## Resource API's
Graph-resources can all be retrieved by Iri (@id) or by Id. Values and edges do mostly not have an Iri (empty-string).
There are API's on each structure-level (resource, value, edge and node). 

The main methods per section:

### Resources API

hasIri
```scala mdoc
graph.resources.hasIri("graph-doc/place/123")
graph.resources.hasIri("graph-doc/place/123", "graph-doc/person/123")
```

hasId
```scala mdoc
graph.resources.hasId(1001L).id
```

upsert
```scala mdoc
graph.values.create("some-literal")
graph.values.create("some-literal") //should be equal because values are deduplicated
graph.values.create("some-literal2") //new value, hence new id
```
count:
```scala mdoc
graph.resources.count()
```

#### Resource
A resource ..
 
### Nodes API
Nodes can be: 

Retrieved by Iri(s)/Uri(s):
```scala mdoc
graph.nodes.hasIri("graph-doc/place/123")
graph.nodes.hasIri("graph-doc/place/123", "graph-doc/person/123")
```
 
Retrieved by Id(s):
```scala mdoc
graph.nodes.hasId(1002L)
```

Counted:
```scala mdoc
graph.nodes.count()
```

#### Node
A node ..

### Edges API
Edges can be:

Counted:
```scala mdoc
graph.edges.count()
```

#### Edge
An edge ..

### Values API

Counted:
```scala mdoc
graph.values.count()
```

#### Value
A value ..
 
## History
 A graph can have a history trait which means that resources are time aware and 
 annotated with supporting "@createdon" and "@deletedon" tags.
 