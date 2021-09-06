package lspace.structure.index

import lspace._
import lspace.Label.P._
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal.UntypedTraversal
import lspace.structure.index.shape.Shape
import monix.eval.Task
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers

trait IndexSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {
  def graph: Graph
  def createIndex(traversal: UntypedTraversal): Task[Index]

  import lspace.Implicits.Scheduler.global

  def indexTests(graph: Graph) =
    "An index".can {
      "test for string-predicates" in {
        (for {
          index <- createIndex(lspace.g.has(`@id`).untyped)
          node1 <- graph.nodes.create()
          node2 <- graph.nodes.create()
          iri1  <- graph.values.create("https://some-example-iri.test")
          iri2  <- graph.values.create("https://some-example-iri.test1")
          _     <- node1 --- `@id` --> iri1
          _     <- node2 --- `@id` --> iri2
          _     <- index.store(Shape(node1))
          _     <- index.store(Shape(node2))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.eqv(iri1.value)))))
            .toListL
            .map(_ shouldBe List(Shape(node1)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.eqv(iri2.value)))))
            .toListL
            .map(_ shouldBe List(Shape(node2)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.prefix(iri1.value.take(5))))))
            .toListL
            .map(_.toSet shouldBe Set(Shape(node1), Shape(node2)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.suffix(iri1.value.takeRight(5))))))
            .toListL
            .map(_ shouldBe List(Shape(node1)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.suffix(iri2.value.takeRight(5))))))
            .toListL
            .map(_ shouldBe List(Shape(node2)))
        } yield succeed).runToFuture
      }
      "test for numeric-predicates".ignore {
        (for {
          index <- createIndex(lspace.__[Any, Any].has(`@id`).untyped)
          node1 <- graph.nodes.create()
          node2 <- graph.nodes.create()
          id1   <- graph.values.create(1L)
          id2   <- graph.values.create(0.4)

          _ <- node1 --- `@id` --> id1
          _ <- node2 --- `@id` --> id2

          _ <- index.store(Shape(node1))
          _ <- index.store(Shape(node2))

          _ <- index
            .find(Vector(Map(`@id` -> List(P.eqv(id1.value)))))
            .toListL
            .map(_ shouldBe List(Shape(node1)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.eqv(id2.value)))))
            .toListL
            .map(_ shouldBe List(Shape(node2)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.gt(0.3)))))
            .toListL
            .map(_.toSet shouldBe Set(Shape(node1), Shape(node2)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.gt(0.8)))))
            .toListL
            .map(_ shouldBe List(Shape(node1)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.lt(0.6)))))
            .toListL
            .map(_ shouldBe List(Shape(node2)))
          _ <- index
            .find(Vector(Map(`@id` -> List(P.between(0.2, 0.6)))))
            .toListL
            .map(_ shouldBe List(Shape(node2)))
        } yield succeed).runToFuture
      }
    }
}
