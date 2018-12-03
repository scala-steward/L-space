package lspace.librarian.process.traversal

import lspace.NS
import java.time._
import lspace.librarian.process.traversal.step.N
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._
import org.scalatest.{Matchers, WordSpec}
import shapeless._

class TraversalSpec extends WordSpec with Matchers {
  val g = DetachedGraph.g

  "A traversal" which {
    "starts empty" in {
      val graphName = "data.example.com/test"
      g.stepsList.size shouldBe 0
      g.self.graph shouldBe DetachedGraph
    }
    "start with a ResourceStep" in {
      g.N.hasLabel(Ontology.ontology).stepsList.size shouldBe 2
      g.E.hasLabel(Property.default.`@label`).stepsList.size shouldBe 2
      g.V.hasLabel(DataType.default.`@string`).stepsList.size shouldBe 2
    }
    "start without a ResourceStep" in {
      g.hasLabel(Ontology.ontology).stepsList.size shouldBe 1
      g.hasLabel(Property.default.`@label`).stepsList.size shouldBe 1
      g.hasLabel(DataType.default.`@string`).stepsList.size shouldBe 1
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
      DetachedGraph.g.N().in().stepsList.size shouldBe 2
      DetachedGraph.g.N().out().stepsList.size shouldBe 2
      DetachedGraph.g.N().out().hasIri("abc").stepsList.size shouldBe 3
      val pDouble =
        Property("schema/x")(_range = () => List(DataType.default.`@double`), containers = List(NS.types.`@list`))
      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
      MemGraphDefault.ns.storeProperty(pDouble)
      //      val pDouble = NumericPropertyKey("x", "schema/x")(TraversalSpec.DoubleType)

      //      println(Traversal.g("biggraph").V().has(pDouble, 0.5).toString)
      //      println(Traversal.g("biggraph").V().has(pDouble, P.eq(0.5).gt(0.4)).toString)
      DetachedGraph.g.N().has(pDouble).stepsList.size shouldBe 2
      val testNode = MemGraphDefault.nodes.create()
      List(1.1, 0.9, 1, 3l).foreach(testNode --- pDouble --> _)
      testNode.addOut(pDouble, 0.5)
      testNode.out(pDouble).size shouldBe 5
      //      testNode.property(pDouble, 1, 1.1, 0.5, 3l)
      //      Traversal[VStep, VStep]().has(NumericPropertyKey("", "")(TraversalSpec.DoubleType), 0L).steps.size shouldBe 1
      import P._
      DetachedGraph.g.N().has(pDouble, P.eqv(1.0)).stepsList.size shouldBe 2
      DetachedGraph.g.N().has(pDouble, P.gte(1.0), P.lt(1.0)).stepsList.size shouldBe 2
      //      DetachedGraph.g.N().has(pDouble, P.gte(1.0) lt (1.0)).steps.size shouldBe 3
      DetachedGraph.g.N().has(pDouble, P.gte(1.0), P.lt(1.0)).stepsList.last.isInstanceOf[step.Has] shouldBe true
      DetachedGraph.g
        .N()
        .has(pDouble, P.gte(1.0), P.lt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .size shouldBe 2
      DetachedGraph.g
        .N()
        .has(pDouble, P.gte(1.2), P.lt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .last
        .assert(1.9) shouldBe false
      DetachedGraph.g
        .N()
        .has(pDouble, P.gte(1.2), P.lt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .last
        .assert(0.9) shouldBe true
      DetachedGraph.g
        .N()
        .has(pDouble, P.lt(1.2), P.gt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .last
        .assert(1.1) shouldBe true
      MemGraphDefault.g.N().out(pDouble).toList.size shouldBe 5
      MemGraphDefault.g.N().has(pDouble, P.eqv(1.1)).toList.size shouldBe 1

      val pString                             = Property("aa")(_range = () => List(DataType.default.`@string`))
      val typedPString: TypedProperty[String] = pString + DataType.default.`@string`
      MemGraphDefault.ns.storeProperty(pString)
      DetachedGraph.g.N().has(pDouble, P.gte("a")).stepsList.size shouldBe 2
    }
    "consist of multiple steps" in {
      val traversal = DetachedGraph.g.N().out().out().in()
      traversal.stepsList.size shouldBe 4
      val pDouble                             = Property("schema/x")(_range = () => List(DataType.default.`@double`))
      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
      val test                                = DetachedGraph.g.N().out(pDouble).hasLabel(DataType.default.`@double`)
      test.sum
      DetachedGraph.g.N().out(pDouble).hasLabel(DataType.default.`@double`).sum
    }
    "which contains labels (as-steps)" can {
      "be selected by valid name" ignore {
        """g.V.as("aname").select("aname")""" should compile
        """g.V.as("aname").select("wrongname")""" shouldNot compile
      }
    }
  }

}
