---
layout: docs
title: Changelog
permalink: docs/changelog
---

# Changelog

## 0.5.0.1
*   added FTraversal which is a the converter for a traversal + graph = result, a ```traversal.withGraph(graph)``` has to following options: 
'headF', 'headOptionF', 'toListF', 'toSetF', 'map' and 'flatMap'. Where 'F' stands for the monad function F[_] from the FTraversal instance. 
When there is a synchronous guide in scope there will also be flat result functions: 'head', 'headOption', 'toList' and 'toSet'
*   added Head- and Last-steps which allow for limiting the results of a (partial-)traversal. 
Result types are depending on the position of using these steps in the traversal. (e.g. ```g.N.group(_.label())``` will result 
in a ```Map[List[Ontology], List[Node]]``` while ```g.N.group(_.label().head)``` will result in a ```Map[Ontology, List[Node]]```)

## 0.5.0.0
*   breaking refactoring!
*   new Guide (traversal-engine interface) and default implementation StandardGuide
*   new Assistent (assertion-engine interface) and default implementation DefaultAssistent
    
*   Predicate logic is now pluggable (e.g. one can implement custom geo-assertions)
    
*   for easy access to Traversal, P, Property.default (Label.P), DataType.default (Label.D):
*   import lspace._

## 0.2.0-SNAPSHOT
*   Added thread-safety to MemGraph and LGraph. Graphs can be modified from multiple threads at once.
*   Implemented Cassandra storage-layer
