package lspace.librarian.structure.index

import lspace.librarian.process.traversal.{P, UntypedTraversal}
import lspace.librarian.structure.index.shape.Shape
import lspace.librarian.structure.{Graph, Property}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

trait IndexSpec extends WordSpec with Matchers with BeforeAndAfterAll {
  def graph: Graph
  def createIndex(traversal: UntypedTraversal): Index

  "An index" can {
    "test for string-predicates" ignore {
      val index = createIndex(graph.__[Any, Any].has(Property.default.`@id`).untyped)

      val node1 = graph.nodes.create()
      val node2 = graph.nodes.create()
      val iri1  = graph.values.create("https://some-example-iri.test")
      val iri2  = graph.values.create("https://some-example-iri.test1")
      val edge1 = node1 --- Property.default.`@id` --> iri1
      val edge2 = node2 --- Property.default.`@id` --> iri2

      index.store(Shape(node1))
      index.store(Shape(node2))

      index.find(Vector(Map(Property.default.`@id` -> List(P.eqv(iri1.value))))) shouldBe List(Shape(node1))
      index.find(Vector(Map(Property.default.`@id` -> List(P.eqv(iri2.value))))) shouldBe List(Shape(node2))
      index
        .find(Vector(Map(Property.default.`@id` -> List(P.prefix(iri1.value.take(5))))))
        .toSet shouldBe Set(Shape(node1), Shape(node2))
      index.find(Vector(Map(Property.default.`@id` -> List(P.suffix(iri1.value.takeRight(5)))))) shouldBe List(
        Shape(node1))

      index.find(Vector(Map(Property.default.`@id` -> List(P.suffix(iri2.value.takeRight(5)))))) shouldBe List(
        Shape(node2))

    }
    "test for numeric-predicates" ignore {
      val index = createIndex(graph.__[Any, Any].has(Property.default.`@id`).untyped)

      val node1 = graph.nodes.create()
      val node2 = graph.nodes.create()
      val id1   = graph.values.create(1l)
      val id2   = graph.values.create(0.4)

      val edge1 = node1 --- Property.default.`@id` --> id1
      val edge2 = node2 --- Property.default.`@id` --> id2

      index.store(Shape(node1))
      index.store(Shape(node2))

      index.find(Vector(Map(Property.default.`@id` -> List(P.eqv(id1.value))))) shouldBe List(Shape(node1))
      index.find(Vector(Map(Property.default.`@id` -> List(P.eqv(id2.value))))) shouldBe List(Shape(node2))
      index
        .find(Vector(Map(Property.default.`@id` -> List(P.gt(0.3)))))
        .toSet shouldBe Set(Shape(node1), Shape(node2))
      index.find(Vector(Map(Property.default.`@id` -> List(P.gt(0.8))))) shouldBe List(Shape(node1))

      index.find(Vector(Map(Property.default.`@id` -> List(P.lt(0.6))))) shouldBe List(Shape(node2))

      index.find(Vector(Map(Property.default.`@id` -> List(P.between(0.2, 0.6))))) shouldBe List(Shape(node2))
    }
  }
}
