---
layout: docs
title: Traversal guide
position: 3
---

# Traversal Guide
* [Overview](#overview)
* [The traverser](#the-traverser)
* [The traversal](#the-traversal)
* [Steps](#steps)
  * [Graph](#graph)
  * [Resource steps](#resource-steps)
    * [N](#n)
    * [E](#e)
    * [V](#v)
    * [R](#r)
  * [Filter steps](#filter-steps)
    * [Has steps](#has-steps)
      * [Has](#has)
      * [HasNot](#hasnot)
      * [HasLabel](#haslabel)
      * [HasId](#hasid)
      * [Hasiri](#hasiri)
    * [Global Filter steps](#global-filter-steps)
      * [Dedup](#dedup)
    * [Coin](#coin)
    * [Where](#where)
    * [And](#and)
    * [Or](#or)
    * [Not](#not)
    * [Is](#is)
  * [Clip steps](#clip-steps)
    * [Range](#range)
    * [Limit](#limit)
    * [Tail](#tail)
  * [Move steps](#move-steps)
    * [Out](#out)
    * [OutE](#oute)
    * [In](#in)
    * [InE](#ine)
    * [Label](#label)
    * [Id](#id)
  * [Branche steps](#branche-steps)
    * [Union](#union)
    * [Local](#local)
    * [Repeat](#repeat)
    * [Coalesce](#coalesce)
  * [Map steps](#map-steps)
    * [Path](#path)
    * [OutMap](#outmap)
    * [OutEMap](#outemap)
    * [InMap](#inmap)
    * [InEMap](#inemap)
  * [Barrier steps](#barrier-steps)
    * [Collecting barrier steps](#collecting-barrier-steps)
      * [Group](#group)
    * [Reducing barrier steps](#reducing-barrier-steps)
      * [Mean](#mean)
      * [Sum](#sum)
      * [Count](#count)
    * [Filter barrier steps](#filter-barrier-steps)
      * [Min](#min)
      * [Max](#max)
    * [Rearrange barrier steps](#rearrange-barrier-steps)
      * [Order](#order)
  * [Side-Effect steps](#side-effect-steps)
    * [Drop](#drop)
  * [Environment steps](#environment-steps)
    * [TimeLimit](#timelimit)
  
```tut:invisible
import lspace._
import Implicits.Scheduler.global
import Implicits.SyncGuide.guide
import lspace.provider.mem.MemGraph
import lspace.structure._
import Label.D._
import lspace.util.SampleGraph
```
## Overview
To acquire knowledge, one must be able to communicate with the Librarian. This document should provide guidance in 
reasoning on a graph.
## The traverser
When a librarian is sent on his way to gather knowledge as instructed with a traversal, the librarian (aka traverser) 
moves through L-space (aka graph(s)) and keeps track of his whereabouts and the encountered resources. When he reaches 
the end of the traversal he communicates back the result that was requested. 
## The traversal
A traversal is an ADT structure which can be compared, concatenated, serialized/deserialized etc.
A traversal exists of steps which are grouped into segments (rules on segmentation: ...). 
A segments starts with a move or transform step (new librarian with new information).
To make a traversal ready for execution add ```.withGraph(graph)``` after the execution. This action will analyse the traversal, 
depending on the nature of the traversal, guide and the result-type the user can do one of the following monadic operations:
* ```.headF```
* ```.headOptionF```
* ```.lastF```
* ```.lastOptionF```
* ```.toListF```
* ```.toSetF```
* ```.toMapF``` this one is only available when there is a Group-step at the root of all container-typed steps

when synchronous operations are available each of these methods has a synchronous counterpart without the 'F'-suffix
## Steps
The librarian is able to execute the following steps.
### Graph
Graph-selection step to point the librarian to a specific part of the L-space 
(we would not want to defy the large quantity of knowledge from the whole multiverse has to offer).
```tut:book
val graph: Graph = MemGraph("librarian-doc")
import scala.concurrent.duration._
scala.concurrent.Await.ready(lspace.util.SampleGraph.loadSocial(graph).runToFuture, 5.seconds)
val labels = SampleGraph.ontologies
val keys = SampleGraph.properties
```
### Resource steps
Resource-step to start specify what type of resources the traversal should start in.
#### N
N-step is a node selection step
#### E
E-step is an edge selection step
#### V
V-step is a value selection step
#### R
R-step is a resource (node, edge and value) selection step
### Filter steps
A filter determines if the traverser should continue with the traversal or not.
#### Has steps
##### Has
Has-step validates the presence of a property and optionally asserts it by one or more predicates
```tut:invisible
import java.time.LocalDate
```
```tut:book
g.N.has("name").out("name").withGraph(graph).toList //filters on nodes which have an edge with property "https://schema.org/name"
g.N.has("name", P.eqv("Garrison")).out("name").withGraph(graph).toList //adds an equality constraint with value "Alice"
g.N.has(keys.birthDate, P.gt(LocalDate.parse("2002-06-13"))).id.withGraph(graph).toList
```
##### HasNot
HasNot-step is the inverse of a Has-step (this could also be written as ```.not(_.has(..))```)
##### HasLabel
HasLabel-step validates the precence of one or more labels (ontology, property or datatype)
```tut:book
g.N.hasLabel(labels.person).out(keys.name).withGraph(graph).toList //filters all persons
g.N.hasLabel(labels.place).out(keys.name).withGraph(graph).toList //filters all places
```
##### HasId
HasId-step validates if the id is within a certain set.
```tut:book
g.N.hasId(1001l).out(keys.name).withGraph(graph).toList
```
##### HasIri
HasIri-step validates if the iri is within a certain set.
```tut:book
g.N.hasIri("graph-doc/place/123").out(keys.name).withGraph(graph).toList
```
#### Global Filter steps
##### Dedup
Dedup-step filters traversers with distinct values
```tut:book
g.N.limit(1).union(_.out().limit(1), _.out().limit(1)).count.withGraph(graph).head //shouldBe 2
g.N.limit(1).union(_.out().limit(1), _.out().limit(1)).dedup().count.withGraph(graph).head //shouldBe 1
```
#### Coin
Coin-step filters traversers on a coin-flip result for probability p
```tut:book
g.N.count.withGraph(graph).head //total nodes to draw from
g.N.coin(0.2).id.withGraph(graph).toList //random selection of nodes with p = 0.2 (20%)
g.N.coin(0.8).id.withGraph(graph).toList //random selection of nodes with p = 0.8 (80%)
```
#### Where
Where-step takes a traversal and filters on non-empty results
```tut:book
g.N.where(_.has("https://schema.org/name")).out("https://schema.org/name").withGraph(graph).toList
```
#### And
And-step takes one or more traversals and filters only those which have non-empty results for all traversals
```tut:book
g.N.and(_.has("https://schema.org/name"), _.has("https://schema.org/birthDate")).out(keys.name, keys.birthDate).withGraph(graph).toList
g.N.and(_.has(keys.balance, P.gt(300)), _.has(keys.balance, P.lt(3000))).count.withGraph(graph).head //shouldBe 2
```
#### Or
Or-step takes one or more traversals and filters only those which have non-empty results for at least one traversal
```tut:book
g.N.or(_.has("https://schema.org/name"), _.has("https://schema.org/birthDate")).out(keys.name, keys.birthDate).withGraph(graph).toList
```
#### Not
Not-step takes a traversal and filters on empty results
```tut:book
g.N.not(_.has("https://schema.org/name")).limit(2).id.withGraph(graph).toList
```
#### Is
Is-step asserts a value against one or more predicates
```tut:book
g.N.out().is(P.eqv(300)).out(keys.name).withGraph(graph).toList //filters the all outgoing resources with value 2
```
### Clip steps
A clip step cuts the resulting stream of traversers.
#### Range
Range-step filters by a low-end and high-end index
```tut:book
g.N.range(4, 16).withGraph(graph).toList //takes only node 4 until 16
```
#### Limit
Limit-step takes the first x-number of traversers
```tut:book
g.N.limit(12).withGraph(graph).toList //takes only the first 12 nodes
```
#### Tail
Tail-step takes the last x-number of traversers
```tut:book
g.N.tail(12).withGraph(graph).toList //takes only the last 12 nodes
```
### Move steps
Move steps lets the traverser move through the graph. The path can be stored within the traverer
#### Out
Out-step takes one or more property-labels and traverses along the valid outgoing paths if any
```tut:book
g.N.out("https://schema.org/name").withGraph(graph).toList
```
#### OutE
OutE-step takes one or more property-labels and traverses to the valid outgoing paths if any
```tut:book
g.N.outE("https://schema.org/name").withGraph(graph).toList
```
#### In
In-step takes one or more property-labels and traverses along the valid incoming paths if any
```tut:book
g.N.out("https://schema.org/name").withGraph(graph).toList
```
#### InE
InE-step takes one or more property-labels and traverses to the valid incoming paths if any
```tut:book
g.N.outE("https://schema.org/name").withGraph(graph).toList
```
#### Label
Label-step traverses to the label-nodes if any
```tut:book
g.N.label().withGraph(graph).toList
```
#### Id
Id-step returns the resource-id (long)
```tut:book
g.N.id.withGraph(graph).toList
```
### Branche steps
Branche steps can execute one or more separate traversals, execute those in a specific way and merges any results back into the original traversal
#### Union
Union-step takes one or more traversals and merges the results into a single traversal
```tut:book
g.N.union(_.out(), _.in()).withGraph(graph).toList //all incoming and outgoing edges
g.N.union(_.has(keys.balance, P.gt(300)), _.has(keys.balance, P.lt(-200))).count.withGraph(graph).head //should be 3
```
#### Local
Local-step takes a traversal and performs any barrier, environment or other more global operations only over the results 
upto the traverser at the root the local-traversal
```tut:book
g.N.local(_.out().hasLabel[Int].sum).withGraph(graph).toList //sums all outgoing edges to integers for each node in the graph
```
#### Repeat
Repeat-step takes a traversal and keeps repeating it until a certain requirement is met, 
a maximum number of repeats is reached or the traversal is exhausted. 
A repeat step returns only the result of the last repeat iteration but can also return the result of 
each intermediary iteration by providing 'collect = true'.
```tut:book
g.N.repeat(_.out("knows")).withGraph(graph).toList //repeats as long as there are outgoing "knows" edges (can easily contain infinite loops)
g.N.repeat(_.out("knows"))(_.hasLabel(Ontology("Officer"))).withGraph(graph).toList //repeats as long as there are outgoing "knows" edges and the result is not of type "Officer"
g.N.repeat(_.out("knows"), 3)(_.hasLabel(Ontology("Officer"))).withGraph(graph).toList //repeats three times at most if there are outgoing "knows" edges and the result is not of type "Officer"
g.N.repeat(_.out("knows"), 3, true)(_.hasLabel(Ontology("Officer"))).withGraph(graph).toList //repeats three times at most if there are outgoing "knows" edges and the result is not of type "Officer" and returns every result in between
g.N.repeat(_.out("knows"), collect = true)(_.hasLabel(Ontology("Officer"))).withGraph(graph).toList //repeats as long as there are outgoing "knows" edges and the result is not of type "Officer" and returns every result in between
g.N.repeat(_.out("knows"), max = 3).withGraph(graph).toList //repeats three times at most if there are outgoing "knows" edges
g.N.repeat(_.out("knows"), max = 3, collect = true).withGraph(graph).toList //repeats three times at most if there are outgoing "knows" edges and returns every result in between
```
#### Coalesce
Coalesce-step takes one or more traversals and returns the result of the first non-empty traversal
```tut:book
g.N.coalesce(_.has(keys.rate, P.gte(4)), _.has(keys.balance, P.lt(-200))).count.withGraph(graph).head //should be 3
```
### Map steps
Map steps ...
#### OutMap
OutMap-step groups the resultset into a ```Map[Property,List[Value]]``` where OutMap is the edge label by which it is grouped and 
```List[Value]``` is the list of values for a certain Property. If the traversal has any succeeding steps after the OutMap-step, 
the traversal will continue to operate with a traverser for each Value. 
```tut:book
g.N.outMap() //returns a property-map on all out-going connected resources
g.N.has("name", P.eqv("Garrison")).outMap().withGraph(graph).head
g.N.outMap("name", "knows") //returns a property-map for edges with label "name" or "knows"
```
#### OutEMap
```tut:book
g.N.has("name", P.eqv("Garrison")).outEMap().withGraph(graph).head //should return all out-going edges grouped by key
```
#### InMap
```tut:book
g.N.has("name", P.eqv("Garrison")).inMap().withGraph(graph).head //returns a property-map on all incoming connected resources
```
#### InEMap
```tut:book
g.N.has("name", P.eqv("Garrison")).inEMap().withGraph(graph).head //should return all in-coming edges grouped by key
```
### Barrier steps
Barrier steps can operate on the entire resultset of a traversal
#### Collecting barrier steps
Collecting steps ...
##### Group
Group-step groups the resultset into a ```Map[Key,List[Value]]``` where Key is the value by which it is grouped and 
```List[Value]``` is the list of values which have the same group-key. If the traversal has any succeeding steps after the Group-step, 
the traversal will continue to operate with a traverser for each Value. 
```tut:book
g.N.group(_.out("name")).withGraph(graph).head //groups only on nodes with a "name" and only takes the first result (head)
g.N.group(_.out("name"))(_.group(_.out("age"))).withGraph(graph).head //can e.g. be a Map[String, List[Map[Int,List[Node]]]]
```
#### Reducing barrier steps
Reducing barrier steps perform a fold task on all traverers in the stream resulting in a single traverser 
with the resulting value of the fold task.
##### Mean
Mean-step passes a traverser where the value is the mean of the values of incoming traversers
```tut:book
g.N.out("balance").hasLabel(`@double`).mean.withGraph(graph).head //should be 624.0225
```
##### Sum
Sum-step passes a traverser where the value is the sum of the values of incoming traversers
```tut:book
g.N.out("balance").hasLabel(`@double`).sum.withGraph(graph).head //should be 2496.09
```
##### Count
Count-step returns the number of incoming traversers
```tut:book
g.N.hasLabel(Ontology("https://schema.org/Person")).count.withGraph(graph).head //should be 6
g.N.hasLabel(Ontology("https://schema.org/Person")).where(_.out(Property("https://schema.org/knows")).count.is(P.gt(1))).count.withGraph(graph).head //should be 5
g.N.hasLabel(Ontology("https://schema.org/Person")).where(_.out(Property("https://schema.org/knows")).count.is(P.lt(2))).count.withGraph(graph).head //should be 1
```
#### Filter barrier steps
Filter barrier steps filters traversers based on some comparison against the complete stream of traversers.
##### Min
Min-step passes only the traverser with the smallest value
```tut:book
g.N.out("balance").hasLabel(`@double`).min.withGraph(graph).head //should be -245.05
g.N.out("balance").hasLabel(`@double`).min.in("balance").out("name").withGraph(graph).head //should be "Levi"
```
##### Max
Max-step passes only the traverser with the largest value
```tut:book
g.N.out("balance").hasLabel(`@double`).max.withGraph(graph).head //should be 2230.30
g.N.out("balance").hasLabel(`@double`).max.in("balance").out("name").withGraph(graph).head //should be "Gray"
```
#### Rearrange barrier steps
Rearrange barrier steps manipulates the position of all the traversers in the stream.
##### Order
Order-step sorts the resultset
### Side-Effect steps
Side-Effect steps ...
#### Drop
Drop-step removes selected resources from the graph (Only for editable graphs)
### Environment steps
Environment steps adjust the context of the traversal
#### TimeLimit
TimeLimit-step limits the amount of time the rest of the traversal may take
