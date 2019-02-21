---
layout: docs
title: Changelog
position: 4
---

# Changelog

## 0.5.0.0-x
*   breaking refactoring!
*   new Guide (traversal-engine interface) and default implementation StandardGuide
*   new Assistent (assertion-engine interface) and default implementation DefaultAssistent
    
*   Predicate logic is now pluggable (e.g. one can implement custom geo-assertions)
    
*   for easy access to Traversal, P, Property.default (Label.P), DataType.default (Label.D):
*   import lspace._

## 0.2.0-SNAPSHOT
*   Added thread-safety to MemGraph and LGraph. Graphs can be modified from multiple threads at once.
*   Implemented Cassandra storage-layer
