package lspace.librarian.traversal

import java.time._

import lspace._
import Label.D._
import lspace.datatype.{EdgeURLType, ListType, NodeURLType, OptionType, TupleType}
import lspace.librarian.logic.{predicate => p}
import lspace.provider.mem.MemGraph
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import shapeless._

import scala.concurrent.Future

class TraversalSpec extends AsyncWordSpec with Matchers {
  val graph = MemGraph("TraversalSpec")

  import lspace.Implicits.Scheduler.global
//  implicit def global: Scheduler = monix.execution.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

//  def testToNode[S <: Traversal[ClassType[Any], ClassType[Any], _ <: HList]](traversal: S)(
//      toTraversal: Node => Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]]) =
//    (for {
//      node         <- traversal.toNode
//      newTraversal <- toTraversal(node)
//    } yield traversal shouldBe newTraversal) //.runToFuture

  "A traversal".which {
    "starts empty" in Future {
//      val graphName = "data.example.com/test"
      g.stepsList.size shouldBe 0
//      g.toNode.graph shouldBe DetachedGraph
    }
    "start with a ResourceStep" in Future {
      g.N.hasLabel(Ontology.ontology).stepsList.size shouldBe 2
      g.E.hasLabel(Property.default.`@label`).stepsList.size shouldBe 2
      g.V.hasLabel(DataType.default.`@string`).stepsList.size shouldBe 2
    }
    "start without a ResourceStep" in Future {
      g.hasLabel(Ontology.ontology).stepsList.size shouldBe 1
      g.hasLabel(Property.default.`@label`).stepsList.size shouldBe 1
      g.hasLabel(DataType.default.`@string`).stepsList.size shouldBe 1
    }
//    "resolve a class-type for .hasLabel(name: String)" in {
//      g.N.hasLabel("Officer")
////      g.N.repeat(_.out("knows"), _.hasLabel(Ontology("Officer")), 3, true)
//    }
    "end-type is numeric".can {
      "be summed up" in Future {
        "g.V.hasLabel[Int].sum()" should compile
        "g.V.hasLabel[Double].sum()" should compile
        "g.V.hasLabel[Long].sum()" should compile
        "g.V.hasLabel[String].sum()" shouldNot compile
        "g.V.hasLabel[Any].sum()" shouldNot compile
      }
      "be averaged" in Future {
        "g.V.hasLabel[Int].mean()" should compile
        "g.V.hasLabel[Double].mean()" should compile
        "g.V.hasLabel[Long].mean()" should compile
        "g.V.hasLabel[String].mean()" shouldNot compile
        "g.V.hasLabel[Any].mean()" shouldNot compile
      }
    }
    "end-type is numeric or temporal".can {
      "be filtered for a max-value" in Future {
        "g.V.hasLabel[Int].max()" should compile
        "g.V.hasLabel[Double].max()" should compile
        "g.V.hasLabel[Long].max()" should compile
        "g.V.hasLabel[Instant].max()" should compile
        "g.V.hasLabel[LocalDate].max()" should compile
        "g.V.hasLabel[LocalTime].max()" should compile
        "g.V.hasLabel[String].max()" shouldNot compile
        "g.V.hasLabel[Any].max()" shouldNot compile
      }
      "be filtered for a min-value" in Future {
        "g.V.hasLabel[Int].min()" should compile
        "g.V.hasLabel[Double].min()" should compile
        "g.V.hasLabel[Long].min()" should compile
        "g.V.hasLabel[Instant].min()" should compile
        "g.V.hasLabel[LocalDate].min()" should compile
        "g.V.hasLabel[LocalTime].min()" should compile
        "g.V.hasLabel[String].min()" shouldNot compile
        "g.V.hasLabel[Any].min()" shouldNot compile
      }
    }
    "start with any step extending TraversalStep".ignore {
      lspace.g.N.in().stepsList.size shouldBe 2
      lspace.g.N.out().stepsList.size shouldBe 2
      lspace.g.N.out().hasIri("abc").stepsList.size shouldBe 3
      val pDouble =
        Property("schema/x")
      pDouble.range + DataType.default.`@double`
//      val typedPDouble: TypedProperty[Double] = pDouble.as(DataType.default.`@double`)
      graph.ns.properties.store(pDouble)
      //      val pDouble = NumericPropertyKey("x", "schema/x")(TraversalSpec.DoubleType)

      //      println(Traversal.g("biggraph").V.has(pDouble, 0.5).toString)
      //      println(Traversal.g("biggraph").V.has(pDouble, P.eq(0.5).gt(0.4)).toString)
      lspace.g.N.has(pDouble).stepsList.size shouldBe 2
//      val testNode = graph.nodes.create()
//      List(1.1, 0.9, 1, 3l).foreach(testNode --- pDouble --> _)
//      testNode.addOut(pDouble, 0.5)
//      testNode.out(pDouble).size shouldBe 5
      //      testNode.property(pDouble, 1, 1.1, 0.5, 3l)
      //      Traversal[VStep, VStep]().has(NumericPropertyKey("", "")(TraversalSpec.DoubleType), 0L).steps.size shouldBe 1
      lspace.g.N.has(pDouble, P.eqv(1.0)).stepsList.size shouldBe 2
      lspace.g.N.has(pDouble, P.gte(1.0) && P.lt(1.0)).stepsList.size shouldBe 2
      //      DetachedGraph.g.N.has(pDouble, P.gte(1.0) lt (1.0)).steps.size shouldBe 3
      lspace.g
        .N
        .has(pDouble, P.gte(1.0) && P.lt(1.0))
        .stepsList
        .last
        .isInstanceOf[step.Has] shouldBe true
      lspace.g
        .N
        .has(pDouble, P.gte(1.0) && P.lt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.size == 2
          case _ => false
        } shouldBe true
      lspace.g
        .N
        .has(pDouble, P.gte(1.2) && P.lt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.head == P.gte(1.2)
          case _ => false
        } shouldBe true
      lspace.g
        .N
        .has(pDouble, P.gte(1.2) && P.lt(1.0))
        .stepsList
        .last
        .asInstanceOf[step.Has]
        .predicate
        .exists {
          case p: p.And => p.predicate.head == P.gte(1.2)
          case _ => false
        } shouldBe true

      val pString = Property("aa")
      pString.range + DataType.default.`@string`

//      val typedPString: TypedProperty[String] = pString.as(DataType.default.`@string`)
      graph.ns.properties.store(pString)
      lspace.g.N.has(pDouble, P.startsWith("a")).stepsList.size shouldBe 2
    }
    "consist of multiple steps" in Future {
      val traversal = lspace.g.N.out().out().in()
      traversal.stepsList.size shouldBe 4
//      val pDouble = Property("schema/x")
//      pDouble.range + DataType.default.`@double`
//      val typedPDouble: TypedProperty[Double] = pDouble + DataType.default.`@double`
//      val test                                = lspace.g.N.out(pDouble).hasLabel(DataType.default.`@double`)
//      test.sum
//      lspace.g.N.out(pDouble).hasLabel(DataType.default.`@double`).sum
    }
//    "contains labels (as-steps)".can {
//      "be selected by valid name".in(Future {
//        """g.V.as("aname").select("aname")""" should compile
//        """g.V.as("aname").select("wrongname")""" shouldNot compile
//      })
//    }
  }

//  "A traversal" must {
//    "have a defined end-type" in {
//  import shapeless.::
//      g.N.outMap().hasLabel(`@int`).et shouldBe `@int`
//    }
//  }
  "A traversal has an expected result type".can {
    """g.out()""" in Future {
      (g.out().et: ClassType[Any]) shouldBe ClassType.stubAny
    }
    """g.N""" in Future {
      (g.N.et: NodeURLType[Node]) shouldBe Node.nodeUrl
    }
    """g.E""" in Future {
      (g.E.et: EdgeURLType[Edge[Any, Any]]) shouldBe Edge.edgeUrl
    }
    """g.N.group(_.out()).mapValues(_.head)""" in Future {
      (g.N
        .group(_.out())
        .mapValues(_.head)
        .et: TupleType[(List[Any], Option[Node])]) shouldBe tupleType(listType(), optionType(Node.nodeUrl))
    }
    """g.N.group(_.outMap())""" in Future {
      (g.N
        .group(_.outMap())
        .et: TupleType[(Map[Property, List[Any]], List[Node])]) shouldBe tupleType(mapType(Property.urlType,
                                                                                           listType()),
                                                                                   listType(Node.nodeUrl))
    }
    """g.N.group(_.out())""" in Future {
      (g.N
        .group(_.out())
        .et: TupleType[(List[Any], List[Node])]) shouldBe tupleType(listType(), listType(Node.nodeUrl))
    }
    """g.E.group(_.out()).mapValues(_.head)""" in Future {
      (g.E
        .group(_.out())
        .mapValues(_.head)
        .et: TupleType[(List[Any], Option[Edge[Any, Any]])]) shouldBe tupleType(listType(), optionType(Edge.edgeUrl))
    }
    """g.E.group(_.out())""" in Future {
      (g.E
        .group(_.out())
        .et: TupleType[(List[Any], List[Edge[Any, Any]])]) shouldBe tupleType(listType(), listType(Edge.edgeUrl))
    }
    """g.V.group(_.out()).mapValues(_.hasLabel[Int].head)""" in Future {
      (g.V
        .group(_.out())
        .mapValues(_.hasLabel[Int].head)
        .et: TupleType[(List[Any], Option[Int])]) shouldBe tupleType(listType(), optionType(`@int`))
    }
    """g.V.group(_.out()).mapValues(_.hasLabel[Int])""" in Future {
      (g.V
        .group(_.out())
        .mapValues(_.hasLabel[Int])
        .et: TupleType[(List[Any], List[Int])]) shouldBe tupleType(listType(), listType(`@int`))
    }
    """g.N.group(_.label()) .mapValues(_.project(_.out("name")).by(_.out("balance").hasLabel[Double].is(P.gt(200.0))))""" in Future {
      (g.N
        .group(_.label())
        .mapValues(_.project(_.out("name")).by(_.out("balance").hasLabel[Double].is(P.gt(200.0))))
        .et: TupleType[(List[Ontology], List[(List[Any], List[Double])])]) shouldBe tupleType(
        listType(Ontology.urlType),
        listType(tupleType(listType(), listType(`@double`))))
    }
    """g.N.project(_.out()).by(_.in())""" in Future {
      (g.N.project(_.out()).by(_.in()).et: TupleType[(List[Any], List[Any])]) shouldBe tupleType(listType(), listType())
    }
    """g.N.project(_.out().hasLabel[Int].head)""" in Future {
      (g.N.project(_.out().hasLabel[Int].head).et: OptionType[Option[Int]]) shouldBe optionType(`@int`)
    }
    """g.N.project(_.out().hasLabel[Int].head).by(_.in())""" in Future {
      (g.N.project(_.out().hasLabel[Int].head).by(_.in()).et: TupleType[(Option[Int], List[Any])]) shouldBe tupleType(
        optionType(`@int`),
        listType())
    }
    """g.N.project(_.group()).by(_.in())""" in Future {
      (g.N
        .project(_.group(_.out()).mapValues(_.out()))
        .by(_.in())
        .et: TupleType[(Map[List[Any], List[Any]], List[Any])]) shouldBe tupleType(mapType(listType(), listType()),
                                                                                   listType())
    }
    """g.N.project(_.out()).by(_.inMap())""" in Future {
      (g.N.project(_.out()).by(_.inMap()).et: TupleType[(List[Any], Map[Property, List[Any]])]) shouldBe tupleType(
        listType(),
        mapType(Property.urlType, listType()))
    }
    """g.N.project(_.out()).by(_.outMap())""" in Future {
      (g.N.project(_.out()).by(_.outMap()).et: TupleType[(List[Any], Map[Property, List[Any]])]) shouldBe tupleType(
        listType(),
        mapType(Property.urlType, listType()))
    }
    """g.N.project(_.out()).by(_.inMap()).by(_.out().hasLabel[Int].max())""" in Future {
      (g.N
        .project(_.out())
        .by(_.inMap())
        .by(_.out().hasLabel[Int].max())
        .et: TupleType[(List[Any], Map[Property, List[Any]], Option[Int])]) shouldBe
        tupleType(listType(), mapType(Property.urlType, listType()), optionType(`@int`))
    }
    """g.hasLabel[...].max()""" in Future {
      g.hasLabel[Int].max().et shouldBe `@int`
      g.hasLabel[Double].max().et shouldBe `@double`
      g.hasLabel[Long].max().et shouldBe `@long`
      g.hasLabel[Instant].max().et shouldBe `@datetime`
      g.hasLabel[LocalDate].max().et shouldBe `@date`
      g.hasLabel[LocalTime].max().et shouldBe `@time`
      g.hasLabel[LocalDateTime].max().et shouldBe `@localdatetime`
    }
    """g.N.max(_....)""" in Future {
      g.N.max(_.out().hasLabel[Int]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[Double]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[Long]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[Instant]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[LocalDate]).et shouldBe NodeURLType.datatype
      g.N.max(_.out().hasLabel[LocalDateTime]).et shouldBe NodeURLType.datatype
    }
    """g.hasLabel[...].min()""" in Future {
      g.hasLabel[Int].min().et shouldBe `@int`
      g.hasLabel[Double].min().et shouldBe `@double`
      g.hasLabel[Long].min().et shouldBe `@long`
      g.hasLabel[Instant].min().et shouldBe `@datetime`
      g.hasLabel[LocalDate].min().et shouldBe `@date`
      g.hasLabel[LocalTime].min().et shouldBe `@time`
      g.hasLabel[LocalDateTime].min().et shouldBe `@localdatetime`
    }
    """g.N.min(_....)""" in Future {
      g.N.min(_.out().hasLabel[Int]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[Double]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[Long]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[Instant]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[LocalDate]).et shouldBe NodeURLType.datatype
      g.N.min(_.out().hasLabel[LocalDateTime]).et shouldBe NodeURLType.datatype
    }
    """g.hasLabel[...].order()""" in Future {
      g.hasLabel[Int].order().et shouldBe `@int`
      g.hasLabel[Double].order().et shouldBe `@double`
      g.hasLabel[Long].order().et shouldBe `@long`
      g.hasLabel[Instant].order().et shouldBe `@datetime`
      g.hasLabel[LocalDate].order().et shouldBe `@date`
      g.hasLabel[LocalTime].order().et shouldBe `@time`
      g.hasLabel[LocalDateTime].order().et shouldBe `@localdatetime`
    }
    """g.N.order(_....)""" in Future {
      g.N.order(_.out().hasLabel[Int]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[Double]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[Long]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[Instant]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[LocalDate]).et shouldBe NodeURLType.datatype
      g.N.order(_.out().hasLabel[LocalDateTime]).et shouldBe NodeURLType.datatype
    }
    """g.N.out().path""" in Future {
      g.N.out().path.et shouldBe ListType()
    }
  }
  "Traversals".can {
    "be compared" in Future {
      g.N.count() shouldBe g.N.count()
      g.N.hasId(1) shouldBe g.N.hasId(1)
      g.N.hasId(1) should not be g.N.hasId(2)
      g.N.has("abc") shouldBe g.N.has("abc")
      g.N.has("abc", P.gt(1)) should not be g.N.has("abc")
      g.N.has("abc") should not be g.N.has("abcd")
      g.N.has("abc") should not be g.N.has("abc").count()

      g.N.has("abc").and(_.out(), _.in()) shouldBe g.N.has("abc").and(_.out(), _.in())
      g.N.has("abc").and(_.out(), _.in()) should not be g.N.has("abc").and(_.out(), _.in().out())
    }
//    "be serialized" in {
//      (for {
//        _ <- testToNode(g.N.has("abc").and(_.out(), _.in()))(Traversal.toTraversal)
//        _ <- testToNode(
//          g.N
//            .has("abc")
//            .and(_.union(_.out(), _.in(Label.P.`@createdon`)), _.id))(Traversal.toTraversal)
//      } yield succeed).runToFuture
//    }
  }
}
