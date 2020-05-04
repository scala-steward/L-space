---
layout: page
section: home
title: "Home"
---
**Welcome to L-space**

[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/L-space/L-space)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/L-space/L-space.svg?columns=all)](https://waffle.io/L-space/L-space)

[![Build Status](https://travis-ci.com/L-space/L-space.svg)](https://travis-ci.com/L-space/L-space)
[![codecov](https://codecov.io/gh/L-space/L-space/branch/master/graph/badge.svg)](https://codecov.io/gh/L-space/L-space)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-brightgreen.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

L-space, a graph computing framework for Scala.

L-space is a graph computing framework WIP with a heavy focus towards Linked (Open) Data. 
The name is inspired by the L-space concept portrayed by Terry Pratchett's Discworld: 
"Because L-space links every library, it is possible to reach any one of these throughout space, 
time and the multiverse (Linked Data?)." Travelling through L-space requires knowledge and ability. 
Hence, steps are defined to instruct the Librarian how to travel.

# Getting started

L-space is available for Scala 2.11.x and 2.12.x. 
To include `lspace-core` (all core structures and basic implementations (in-memory) of the graph, 
data-models and traversal-engines) add the following to your `build.sbt`:
```
libraryDependencies += "eu.l-space" %% "lspace-core" % "{{ site.data.settings.version }}"
```
Basic minimal import:
```
import lspace._
import lspace.Implicits.StandardGuide.guide
```

# Modules

L-space provides additional modules for remote traversals, parsing io, rest-api templates and tinkerpop-adapters.

- `lspace-parse`: parsers for linked data
- `lspace-client`: clients to communicatie with other graph(-services)
- `lspace-graph`: implementation for persistence and indexing remote (e.g. Cassandra, Elasticsearch, HDFS, ...)
- `lspace-services`: basic implementation for a rest service which accepts Librarian queries in Json-LD and returns results in Json-LD

```
libraryDependencies += "eu.l-space" %% "{lspace-xxx}" % "{{ site.data.settings.version }}"
libraryDependencies += "eu.l-space" %% "{lspace-xxx-tests}" % "{{ site.data.settings.version }}"
```

## Examples
[L-space-examples](https://github.com/ThijsBroersen/L-space-examples) on how to use the project modules in various environments and 
sample-environments (e.g. Jupyter-notebook with samples)