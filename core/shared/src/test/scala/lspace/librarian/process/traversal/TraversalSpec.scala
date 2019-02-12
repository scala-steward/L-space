package lspace.librarian.process.traversal

import java.time._
import lspace.NS
import lspace.librarian.datatype.DataType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure._
import org.scalatest.{Matchers, WordSpec}
import shapeless._

class TraversalSpec extends WordSpec with Matchers {
  val graph = MemGraph("TraversalSpec")
  val g     = graph.g

  "A traversal" which {
    "starts empty" in {
      val graphName = "data.example.com/test"
      g.segmentList.flatMap(_.stepsList).size shouldBe 0
      g.toNode.graph shouldBe DetachedGraph
      g.target shouldBe graph
    }
    "start with a ResourceStep" in {
      g.N.hasLabel(Ontology.ontology).segmentList.flatMap(_.stepsList).size shouldBe 2
      g.E.hasLabel(Property.default.`@label`).segmentList.flatMap(_.stepsList).size shouldBe 2
      g.V.hasLabel(DataType.default.`@string`).segmentList.flatMap(_.stepsList).size shouldBe 2
    }
    "start without a ResourceStep" in {
      g.hasLabel(Ontology.ontology).segmentList.flatMap(_.stepsList).size shouldBe 1
      g.hasLabel(Property.default.`@label`).segmentList.flatMap(_.stepsList).size shouldBe 1
      g.hasLabel(DataType.default.`@string`).segmentList.flatMap(_.stepsList).size shouldBe 1
    }
    "end-type is numeric" can {
      "be summed up" in {
        "g.V.hasLabel[Int].sum" should compile
        "g.V.hasLabel[Double].sum" should compile
        "g.V.hasLabel[Long].sum" should compile
        "g.V.hasLabel[String].sum" shouldNot compile
        "g.V.hasLabel[Any].sum" shouldNot compile
      }
      "be averaged" in {
        "g.V.hasLabel[Int].mean" should compile
        "g.V.hasLabel[Double].mean" should compile
        "g.V.hasLabel[Long].mean" should compile
        "g.V.hasLabel[String].mean" shouldNot compile
        "g.V.hasLabel[Any].mean" shouldNot compile
      }
    }
    "end-type is numeric or temporal" can {
      "be filtered for a max-value" in {
        "g.V.hasLabel[Int].max" should compile
        "g.V.hasLabel[Double].max" should compile
        "g.V.hasLabel[Long].max" should compile
        "g.V.hasLabel[Instant].max" should compile
        "g.V.hasLabel[LocalDate].max" should compile
        "g.V.hasLabel[LocalTime].max" should compile
        "g.V.hasLabel[String].max" shouldNot compile
        "g.V.hasLabel[Any].max" shouldNot compile
      }
      "be filtered for a min-value" in {
        "g.V.hasLabel[Int].min" should compile
        "g.V.hasLabel[Double].min" should compile
        "g.V.hasLabel[Long].min" should compile
        "g.V.hasLabel[Instant].min" should compile
        "g.V.hasLabel[LocalDate].min" should compile
        "g.V.hasLabel[LocalTime].min" should compile
        "g.V.hasLabel[String].min" shouldNot compile
        "g.V.hasLabel[Any].min" shouldNot compile
      }
    }
    "start with any step extending TraversalStep" in {
      graph.g.N().in().segmentList.flatMap(_.stepsList).size shouldBe 2
      graph.g.N().out().segmentList.flatMap(_.stepsList).size shouldBe 2
      graph.g.N().out().hasIri("abc").segmentList.flatMap(_.stepsList).size shouldBe 3
      val pDouble =
        Property._Property("schema/x")(_range = () => List(DataType.default.`@double`),
                                       containers = List(NS.types.`@list`))
      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
      graph.ns.properties.store(pDouble)
      //      val pDouble = NumericPropertyKey("x", "schema/x")(TraversalSpec.DoubleType)

      //      println(Traversal.g("biggraph").V().has(pDouble, 0.5).toString)
      //      println(Traversal.g("biggraph").V().has(pDouble, P.eq(0.5).gt(0.4)).toString)
      graph.g.N().has(pDouble).segmentList.flatMap(_.stepsList).size shouldBe 2
      val testNode = graph.nodes.create()
      List(1.1, 0.9, 1, 3l).foreach(testNode --- pDouble --> _)
      testNode.addOut(pDouble, 0.5)
      testNode.out(pDouble).size shouldBe 5
      //      testNode.property(pDouble, 1, 1.1, 0.5, 3l)
      //      Traversal[VStep, VStep]().has(NumericPropertyKey("", "")(TraversalSpec.DoubleType), 0L).steps.size shouldBe 1
      import P._
      graph.g.N().has(pDouble, P.eqv(1.0)).segmentList.flatMap(_.stepsList).size shouldBe 2
      graph.g.N().has(pDouble, P.gte(1.0) && P.lt(1.0)).segmentList.flatMap(_.stepsList).size shouldBe 2
      //      DetachedGraph.g.N().has(pDouble, P.gte(1.0) lt (1.0)).steps.size shouldBe 3
      graph.g
        .N()
        .has(pDouble, P.gte(1.0) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .isInstanceOf[step.Has] shouldBe true
      graph.g
        .N()
        .has(pDouble, P.gte(1.0) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.size == 2
        } shouldBe true
      graph.g
        .N()
        .has(pDouble, P.gte(1.2) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.head.assert(1.9)
        } shouldBe true
      graph.g
        .N()
        .has(pDouble, P.gte(1.2) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.last.assert(0.9)
        } shouldBe true
      graph.g.N().out(pDouble).toList.size shouldBe 5
      graph.g.N().has(pDouble, P.eqv(1.1)).toList.size shouldBe 1

      val pString                             = Property._Property("aa")(_range = () => List(DataType.default.`@string`))
      val typedPString: TypedProperty[String] = pString + DataType.default.`@string`
      graph.ns.properties.store(pString)
      graph.g.N().has(pDouble, P.startsWith("a")).segmentList.flatMap(_.stepsList).size shouldBe 2
    }
    "consist of multiple steps" in {
      val traversal = graph.g.N().out().out().in()
      traversal.segmentList.flatMap(_.stepsList).size shouldBe 4
      val pDouble                             = Property._Property("schema/x")(_range = () => List(DataType.default.`@double`))
      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
      val test                                = graph.g.N().out(pDouble).hasLabel(DataType.default.`@double`)
      test.sum
      graph.g.N().out(pDouble).hasLabel(DataType.default.`@double`).sum
    }
    "contains labels (as-steps)" can {
      "be selected by valid name" ignore {
        """g.V.as("aname").select("aname")""" should compile
        """g.V.as("aname").select("wrongname")""" shouldNot compile
      }
    }
  }

  "Traversals" can {
    "be compared" in {
      g.N().count shouldBe g.N().count
      g.N().hasId(1) shouldBe g.N().hasId(1)
      g.N().hasId(1) should not be g.N().hasId(2)
      g.N.has("abc") shouldBe g.N.has("abc")
      g.N.has("abc", P.gt(1)) should not be g.N.has("abc")
      g.N.has("abc") should not be g.N.has("abcd")
      g.N.has("abc") should not be g.N.has("abc").count

      g.N.has("abc").and(_.out(), _.in()) shouldBe g.N.has("abc").and(_.out(), _.in())
      g.N.has("abc").and(_.out(), _.in()) should not be g.N.has("abc").and(_.out(), _.in().out())
    }
  }
}
