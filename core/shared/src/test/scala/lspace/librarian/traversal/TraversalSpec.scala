package lspace.librarian.traversal

import java.time._

import lspace._
import Label.D._
import lspace.datatype.{IntType, NodeURLType}
import lspace.librarian.logic.{predicate => p}
import lspace.provider.mem.MemGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, Matchers}
import shapeless._

class TraversalSpec extends AsyncWordSpec with Matchers {
  val graph = MemGraph("TraversalSpec")

  import lspace.Implicits.Scheduler.global

  def testToNode[S <: Traversal[ClassType[Any], ClassType[Any], HList]](traversal: S)(toTraversal: Node => Task[S]) =
    (for {
      node         <- traversal.toNode
      newTraversal <- toTraversal(node)
    } yield traversal shouldBe newTraversal).runToFuture

  "A traversal" which {
    "starts empty" in {
      val graphName = "data.example.com/test"
      g.segmentList.flatMap(_.stepsList).size shouldBe 0
//      g.toNode.graph shouldBe DetachedGraph
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
//    "resolve a class-type for .hasLabel(name: String)" in {
//      g.N.hasLabel("Officer")
////      g.N.repeat(_.out("knows"), _.hasLabel(Ontology("Officer")), 3, true)
//    }
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
      lspace.g.N().in().segmentList.flatMap(_.stepsList).size shouldBe 2
      lspace.g.N().out().segmentList.flatMap(_.stepsList).size shouldBe 2
      lspace.g.N().out().hasIri("abc").segmentList.flatMap(_.stepsList).size shouldBe 3
      val pDouble =
        Property("schema/x")
      pDouble.range + DataType.default.`@double`
      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
      graph.ns.properties.store(pDouble)
      //      val pDouble = NumericPropertyKey("x", "schema/x")(TraversalSpec.DoubleType)

      //      println(Traversal.g("biggraph").V().has(pDouble, 0.5).toString)
      //      println(Traversal.g("biggraph").V().has(pDouble, P.eq(0.5).gt(0.4)).toString)
      lspace.g.N().has(pDouble).segmentList.flatMap(_.stepsList).size shouldBe 2
      val testNode = graph.nodes.create()
//      List(1.1, 0.9, 1, 3l).foreach(testNode --- pDouble --> _)
//      testNode.addOut(pDouble, 0.5)
//      testNode.out(pDouble).size shouldBe 5
      //      testNode.property(pDouble, 1, 1.1, 0.5, 3l)
      //      Traversal[VStep, VStep]().has(NumericPropertyKey("", "")(TraversalSpec.DoubleType), 0L).steps.size shouldBe 1
      lspace.g.N().has(pDouble, P.eqv(1.0)).segmentList.flatMap(_.stepsList).size shouldBe 2
      lspace.g.N().has(pDouble, P.gte(1.0) && P.lt(1.0)).segmentList.flatMap(_.stepsList).size shouldBe 2
      //      DetachedGraph.g.N().has(pDouble, P.gte(1.0) lt (1.0)).steps.size shouldBe 3
      lspace.g
        .N()
        .has(pDouble, P.gte(1.0) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .isInstanceOf[step.Has] shouldBe true
      lspace.g
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
      lspace.g
        .N()
        .has(pDouble, P.gte(1.2) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.head == P.gte(1.2)
        } shouldBe true
      lspace.g
        .N()
        .has(pDouble, P.gte(1.2) && P.lt(1.0))
        .segmentList
        .flatMap(_.stepsList)
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.head == P.gte(1.2)
        } shouldBe true

      val pString = Property("aa")
      pString.range + DataType.default.`@string`

      val typedPString: TypedProperty[String] = pString + DataType.default.`@string`
      graph.ns.properties.store(pString)
      lspace.g.N().has(pDouble, P.startsWith("a")).segmentList.flatMap(_.stepsList).size shouldBe 2
    }
    "consist of multiple steps" in {
      val traversal = lspace.g.N().out().out().in()
      traversal.segmentList.flatMap(_.stepsList).size shouldBe 4
//      val pDouble = Property("schema/x")
//      pDouble.range + DataType.default.`@double`
//      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
//      val test                                = lspace.g.N().out(pDouble).hasLabel(DataType.default.`@double`)
//      test.sum
//      lspace.g.N().out(pDouble).hasLabel(DataType.default.`@double`).sum
    }
    "contains labels (as-steps)" can {
      "be selected by valid name" ignore {
        """g.V.as("aname").select("aname")""" should compile
        """g.V.as("aname").select("wrongname")""" shouldNot compile
      }
    }
  }

  "A traversal" must {
//    "have a defined end-type" in {
    import shapeless.::
//      g.N.outMap().hasLabel(`@int`).et shouldBe `@int`
//    }
  }
  "A traversal has an expected result type" can {
    """g.out()""" in {
      g.out().et shouldBe ClassType.stubAny
    }
    """g.N""" in {
      g.N.et shouldBe Node.nodeUrl
    }
    """g.E""" in {
      g.E.et shouldBe Edge.edgeUrl
    }
    """g.N.group(_.out()).mapValues(_.head)""" in {
      g.N
        .group(_.out())
        .mapValues(_.head)
        .et shouldBe tupleType(listType(), optionType(Node.nodeUrl))
    }
    """g.N.group(_.out())""" in {
      g.N
        .group(_.out())
        .et shouldBe tupleType(listType(), listType(Node.nodeUrl))
    }
    """g.E.group(_.out()).mapValues(_.head)""" in {
      g.E
        .group(_.out())
        .mapValues(_.head)
        .et shouldBe tupleType(listType(), optionType(Edge.edgeUrl))
    }
    """g.E.group(_.out())""" in {
      g.E
        .group(_.out())
        .et shouldBe tupleType(listType(), listType(Edge.edgeUrl))
    }
    """g.V.group(_.out()).mapValues(_.hasLabel[Int].head)""" in {
      g.V
        .group(_.out())
        .mapValues(_.hasLabel[Int].head)
        .et shouldBe tupleType(listType(), optionType(`@int`))
    }
    """g.V.group(_.out()).mapValues(_.hasLabel[Int])""" in {
      g.V
        .group(_.out())
        .mapValues(_.hasLabel[Int])
        .et shouldBe tupleType(listType(), listType(`@int`))
    }
    """g.N.group(_.label()) .mapValues(_.project(_.out("name")).by(_.out("balance").hasLabel[Double].is(P.gt(200.0))))""" in {
      g.N
        .group(_.label())
        .mapValues(_.project(_.out("name")).by(_.out("balance").hasLabel[Double].is(P.gt(200.0))))
        .et shouldBe tupleType(listType(Ontology.urlType), listType(tupleType(listType(), listType(`@double`))))
    }
    """g.N.project(_.out()).by(_.in())""" in {
      g.N.project(_.out()).by(_.in()).et shouldBe tupleType(listType(), listType())
    }
    """g.N.project(_.out().hasLabel[Int].head)""" in {
      g.N.project(_.out().hasLabel[Int].head).et shouldBe tupleType(optionType(`@int`))
    }
    """g.N.project(_.out().hasLabel[Int].head).by(_.in())""" in {
      g.N.project(_.out().hasLabel[Int].head).by(_.in()).et shouldBe tupleType(optionType(`@int`), listType())
    }
    """g.N.project(_.out()).by(_.inMap())""" in {
      g.N.project(_.out()).by(_.inMap()).et shouldBe tupleType(listType(),
                                                               listType(mapType(Property.urlType, listType())))
    }
    """g.N.project(_.out()).by(_.inMap()).by(_.outMap())""" in {
      g.N.project(_.out()).by(_.inMap()).by(_.outMap()).et shouldBe
        tupleType(listType(),
                  listType(mapType(Property.urlType, listType())),
                  listType(mapType(Property.urlType, listType())))
    }
    """g.hasLabel[...].max()""" in {
      g.hasLabel[Int].max().et shouldBe `@int`
      g.hasLabel[Double].max().et shouldBe `@double`
      g.hasLabel[Long].max().et shouldBe `@long`
      g.hasLabel[Instant].max().et shouldBe `@datetime`
      g.hasLabel[LocalDate].max().et shouldBe `@date`
      g.hasLabel[LocalTime].max().et shouldBe `@time`
      g.hasLabel[LocalDateTime].max().et shouldBe `@localdatetime`
    }
    """g.N.max(_....)""" in {
      g.N.max(_.out().hasLabel[Int]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[Double]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[Long]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[Instant]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[LocalDate]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[LocalDateTime]).et shouldBe NodeURLType.datatype
    }
    """g.hasLabel[...].min()""" in {
      g.hasLabel[Int].min().et shouldBe `@int`
      g.hasLabel[Double].min().et shouldBe `@double`
      g.hasLabel[Long].min().et shouldBe `@long`
      g.hasLabel[Instant].min().et shouldBe `@datetime`
      g.hasLabel[LocalDate].min().et shouldBe `@date`
      g.hasLabel[LocalTime].min().et shouldBe `@time`
      g.hasLabel[LocalDateTime].min().et shouldBe `@localdatetime`
    }
    """g.N.min(_....)""" in {
      g.N.min(_.out().hasLabel[Int]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[Double]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[Long]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[Instant]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[LocalDate]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[LocalDateTime]).et shouldBe NodeURLType.datatype
    }
    """g.hasLabel[...].order()""" in {
      g.hasLabel[Int].order().et shouldBe `@int`
      g.hasLabel[Double].order().et shouldBe `@double`
      g.hasLabel[Long].order().et shouldBe `@long`
      g.hasLabel[Instant].order().et shouldBe `@datetime`
      g.hasLabel[LocalDate].order().et shouldBe `@date`
      g.hasLabel[LocalTime].order().et shouldBe `@time`
      g.hasLabel[LocalDateTime].order().et shouldBe `@localdatetime`
    }
    """g.N.order(_....)""" in {
      g.N.order(_.out().hasLabel[Int]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[Double]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[Long]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[Instant]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[LocalDate]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[LocalDateTime]).et shouldBe NodeURLType.datatype
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
    "be serialized" in {
      for {
        _ <- testToNode(
          g.N.has("abc").and(_.out(), _.in()).asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])(
          Traversal.toTraversal)
        _ <- testToNode(
          g.N
            .has("abc")
            .and(_.union(_.out(), _.in(Label.P.`@createdon`)), _.id)
            .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])(Traversal.toTraversal)
      } yield succeed
    }
  }
}
