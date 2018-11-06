---
layout: page
section: home
title: "Home"
---
**Welcome to L-space**

L-space, a graph computing framework for Scala.

L-space is a graph computing framework WIP with a heavy focus towards Linked (Open) Data. 
The name is inspired by the L-space concept portrayed by Terry Pratchett's Discworld: 
"Because L-space links every library, it is possible to reach any one of these throughout space, 
time and the multiverse (Linked Data?)." Travelling through L-space requires knowledge and ability. 
Hence, steps are defined to instruct the Librarian how to travel.

# Getting started

L-space modules are available via jitpack.io. Add it in your `build.sbt` at the end of resolvers:
```
resolvers += "jitpack" at "https://jitpack.io"
```
L-space is available for Scala 2.11.x and 2.12.x. 
To include `lspace-core` (all core structures and basic implementations (in-memory) of the graph, 
data-models and traversal-engines) add the following to your `build.sbt`:
```
libraryDependencies += "eu.l-space" %% "lspace-core" % "{{ site.data.settings.version }}"
```

# Modules

L-space provides additional modules for remote traversals, parsing io, rest-api templates and tinkerpop-adapters.

- `lspace-parse`: parsers for linked data
- `lspace-client`: clients to communicatie with other graph(-services)
- `lspace-graph`: basic implementation for persistence and indexing remote (e.g. Cassandra, Elasticsearch, HDFS, ...)
- `lspace-services`: basic implementation for a rest service which accepts Librarian queries in Json-LD and returns results in Json-LD

```
libraryDependencies += "eu.l-space" %% "{lspace-xxx}" % "{{ site.data.settings.version }}"
libraryDependencies += "eu.l-space" %% "{lspace-xxx-tests}" % "{{ site.data.settings.version }}"
```
