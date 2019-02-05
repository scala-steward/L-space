package lspace.librarian.process.computer

import java.time.LocalDate

import lspace.librarian.datatype._
import DataType.default._
import lspace.librarian.process.traversal._
import lspace.librarian.util.SampleGraph
import org.scalatest.BeforeAndAfterAll
import lspace.librarian.structure._
import lspace.types.vector.Point
import org.scalatest.{Matchers, WordSpec}

import scala.language._

trait GraphComputerSpec extends WordSpec with Matchers with BeforeAndAfterAll {

  implicit def sampleGraph: Graph
  lazy val g = sampleGraph.g

  val properties = SampleGraph.properties
  val ontologies = SampleGraph.ontologies
  val namespaces = SampleGraph.namespaces

  def perform = afterWord("perform")

  override def beforeAll = {
    SampleGraph.loadSocial(sampleGraph)
  }

  "A GraphComputer" can perform {
    "a N-step" in {
      val nodes = g.N.toStream
      nodes.nonEmpty shouldBe true
      nodes.forall(_.isInstanceOf[Node]) should be(true)
    }
    "a E-step" in {
      val edge = g.E.toList //implicit WithTraversalStream not resolved by IntelliJ IDEA, toList not recognized
//      edge.head.id shouldBe 3l
      //      val edges = g.E(edge).toList
      //      edges.size shouldBe 1
      //      edges.head.id shouldBe edge.id
    }
    "a V-step" in {
      val value = g.V.head
//      val values = g.V(value).toList
//      values.head shouldBe value
    }
    "a R-step" in {
      //      val resource = g.R.head
      //      val resources = g.R(resource).toList
      //      resources.head.id shouldBe resource.id
    }
    "a Out-step" in {
      val values = g.N.out().toStream
      values.nonEmpty shouldBe true
      g.N.has("name", P.eqv("Garrison")).out("name").head shouldBe "Garrison"
    }
    "a OutMap-step" in {
      Traversal.WithTraversalStreamTyped(g.N.outMap()).toStream.head
      import shapeless.::
      val b: IntType[Int] =
        g.N.outMap().hasLabel(`@int`).et
      val c: DataType[Graph] =
        g.N.outMap().hasLabel(`@int`).st

      Traversal.WithTraversalStreamTyped(g.N.outMap().hasLabel(`@int`)).toStream.head
      g.N.outMap().hasLabel(`@int`).toStream.head
      g.N.outMap().toStream.nonEmpty shouldBe true
      g.N.has("name", P.eqv("Garrison")).outMap().head.size shouldBe 5
    }
    "a OutE-step" in {
      val values = g.N.outE().toStream
      values.nonEmpty shouldBe true
      values.take(1).exists(_.isInstanceOf[Edge[_, _]]) should be(true)
      g.N.has("name").outE("name").head.key.iri shouldBe "name"
    }
    "a OutEMap-step" in {
      g.N.outEMap().toStream.nonEmpty shouldBe true
    }
    "a In-step" in {
      g.V("Garrison").toList.nonEmpty should be(true)
      g.V.is(P.eqv("Garrison")).toList.nonEmpty should be(true)
      g.V.is(P.eqv("Garrison")).in("name").out(Property.default.`@id`).head shouldBe (sampleGraph.iri + "/person/56789")
    }
    "a InMap-step" in {
      val values = g.N.inMap().toStream
      values.head.isInstanceOf[Map[Property, Any]] shouldBe true
      values.nonEmpty shouldBe true
    }
    "a InE-step" in {
      val values = g.N.inE().toStream
      values.nonEmpty shouldBe true
      values.take(1).exists(_.isInstanceOf[Edge[_, _]]) should be(true)
      g.V.is(P.eqv("Garrison")).inE("name").head.key.iri shouldBe "name"
    }
    "a InEMap-step" in {
      g.N.inEMap().toStream.nonEmpty shouldBe true
    }
    "a Has-step" when {
      "temporal predicate" in {
        g.N.has(properties.birthDate).count().head shouldBe 6
        g.N.has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13"))).count.head shouldBe 2
        g.N.has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13"))).count.head shouldBe 3
        g.N.has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13"))).count.head shouldBe 3
        g.N.has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13"))).count.head shouldBe 4
        g.N
          .has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .head shouldBe 2
        g.N
          .has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .head shouldBe 3
        g.N
          .has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .head shouldBe 3
      }

      "numeric predicate" in {
        g.N.has(properties.balance).count.head shouldBe 5
        g.N.has(properties.balance, P.gt(300)).count.head shouldBe 2
        g.N.has(properties.balance, P.gt(300.0)).count.head shouldBe 2
        g.N.has(properties.balance, P.gte(300)).count.head shouldBe 3
        g.N.has(properties.balance, P.lt(300)).count.head shouldBe 2
        g.N.has(properties.balance, P.lte(300)).count.head shouldBe 3
        g.N.has(properties.balance, P.inside(300, 3000)).count.head shouldBe 2
        g.N.has(properties.balance, P.inside(300, 3000.5)).count.head shouldBe 2
        g.N.has(properties.balance, P.outside(300, 3000)).count.head shouldBe 2
        g.N.has(properties.balance, P.between(300, 3000)).count.head shouldBe 3
      }

      "geometric predicate" in {
        g.N.has(properties.geo, P.within(Point(72.0403, 60.90879))).count.head shouldBe 1
      }
    }
    "a HasNot-step" in {
      g.N.hasNot(Property.default.`@label`).toStream.nonEmpty shouldBe true
    }
    "a HasId-step" in {
      val someId = g.N.hasIri(sampleGraph.iri + "/place/123").id.head
      g.N.hasId(someId).out(Property.default.`@id`).head shouldBe sampleGraph.iri + "/place/123"
    }
    "a HasIri-step" in {
      g.N.hasIri(sampleGraph.iri + "/place/123").toStream.nonEmpty shouldBe true
    }
    "a Coin-step" in {
      g.N.coin(0.0).toStream.isEmpty shouldBe true
      g.N.coin(1.0).toStream.nonEmpty shouldBe true
    }
    "a Path-step" in {
      Traversal
        .WithTraversalStreamTyped(
          g.N
            .out("https://schema.org/knows")
            .out("https://schema.org/knows")
            .path(_.out("name")))
        .toStream
        .head
      g.N
        .out("https://schema.org/knows")
        .out("https://schema.org/knows")
        .path(_.out("name"))
        .toStream
        .head
        .size shouldBe 2
    }
    "a Where-step" in {
      g.N.where(_.has(properties.balance)).out(properties.name).toStream.nonEmpty shouldBe true
    }
    "a And-step" in {
      g.N.and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000))).count.head shouldBe 2
    }
    "a Or-step" in {
      g.N.or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count.head shouldBe 3
    }
    "a Union-step" in {
      g.N.union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count.head shouldBe 3

//      Traversal.WithTraversalStream(g.V.hasLabel(listType[Double])).toList
//      g.N.out().hasLabel(listType[Double]).toList
//      g.N.out().hasLabel(listType[Double], listType[Int]).toList.head
//      Traversal.WithTraversalStream(g.N.out().hasLabel(listType[Double], listType[Int])).toList
//      g.N.out().hasLabel(listType(), vectorType()).toList.head
//      g.N.out().hasLabel(intType, doubleType, intType, doubleType, intType, doubleType).et
//      g.N.out().hasLabel(intType, doubleType, dateTimeType).et
//      g.N.out().hasLabel(geopointType, dateTimeType, doubleType).et
    }
    "a Local-step" in {
//      g.N.hasLabel(ontologies.person).local(_.out(properties.knows).count).toList shouldBe List(1, 3, 2, 2, 2, 2)
      g.N.hasLabel(ontologies.person).local(_.out(properties.name).count).toList shouldBe List(1, 1, 1, 1, 1, 1)
    }
    "A Coalesce-step" in {
      g.N.coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200))).count.head shouldBe 3
    }
    "a Not-step" in {
      g.N.not(_.has(Property.default.`@label`)).toStream.nonEmpty shouldBe true
    }
    "a Project-step" in {
      g.N.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`)).toStream.nonEmpty shouldBe true
      g.N
        .union(
          _.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`)),
          _.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`))
        )
        .toStream
        .nonEmpty shouldBe true
    }
    "a Group-step" in {
      val groupedNodes: Stream[Map[List[Ontology], List[Node]]] = g.N.group(_.label()).toStream
      g.N.group(_.label()).toStream
      g.N.label()
      groupedNodes.nonEmpty shouldBe true
      val test = g.N.group(_.label())
      test.out("abc").out("def")

      val doubleGroupedNodes: Stream[Map[List[Ontology], List[Map[Property, List[Any]]]]] =
        g.N.group(_.label()).outMap().toStream
      doubleGroupedNodes.nonEmpty shouldBe true

      val doubledoubleGroupedNodes: Stream[Map[List[Ontology], List[Map[Property, List[Map[Property, List[Any]]]]]]] =
        g.N.group(_.label()).outMap().outMap().toStream
      doubleGroupedNodes.nonEmpty shouldBe true
    }
    "a Drop-step" ignore {
      val p         = sampleGraph + Ontology("https://schema.org/Person")
      val weirdname = "lkaskfdmnowenoiafps"
      p --- "name" --> weirdname
      g.N.has("name", P.eqv(weirdname)).count.head shouldBe 1
      g.N.has("name", P.eqv(weirdname)).drop().iterate()
      g.N.has("name", P.eqv(weirdname)).count.head shouldBe 0
      p.remove()
    }
    "a Dedup-step" in {
      g.N.limit(1).union(_.out().limit(1), _.out().limit(1)).count.head shouldBe 2
      g.N.limit(1).union(_.out().limit(1), _.out().limit(1)).dedup().count.head shouldBe 1
      g.N.limit(1).union(_.out().limit(2), _.out().limit(2)).count.head shouldBe 4
      g.N.limit(1).union(_.out().limit(2), _.out().limit(2)).dedup().count.head shouldBe 2
    }

    "a Is-step" which {
      "has an equivalent node-type value" in {
        val levi = g.N.hasIri(sampleGraph.iri + "/person/12345").head
        g.N.is(P.eqv(levi)).toList.size shouldBe 1
      }
      "has an equal value-type value" in {
        g.V.is(P.eqv(sampleGraph.iri + "/place/123")).toList.size shouldBe 1
      }
      "has a greater than value-type value" in {
        g.V.is(P.contains("place/1234")).toList.size shouldBe 1
        g.V.is(P.prefix(sampleGraph.iri + "/place/1234")).toList.size shouldBe 1
        g.V.is(P.suffix("ace/12345")).toList.size shouldBe 1
      }
    }
    "a HasLabel-step" in {
      g.N.hasLabel(ontologies.place).toList.size should be > 0
      g.V.hasLabel(`@int`).toList.size shouldBe 4
      g.V.hasLabel(`@double`).toList.size shouldBe 4
      g.V.hasLabel(`@string`).toList.size shouldBe 20
      g.V.hasLabel(`@int`, `@double`).toList.size shouldBe 8
      g.V.hasLabel(`@int`, `@double`, `@string`).toList.size shouldBe 28
      g.V.hasLabel[Int].toList.size shouldBe 4
      g.V.hasLabel("@int").toList.size shouldBe 4
    }
    "a Order-step" in {
//      g.N.order(_.out("name").hasLabel[String]).local(_.out("name").limit(1)).head shouldBe "Crystal Springs"
      g.N.order(_.out("name").hasLabel(`@string`)).local(_.out("name").limit(1)).head shouldBe "Crystal Springs"
      g.N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("balance").head shouldBe 2230.30
//      g.N.order(_.out("balance").hasLabel[Double], false).limit(1).out("balance").head shouldBe 2230.30
      g.N.order(_.out("balance").hasLabel(`@double`)).limit(1).out("balance").head shouldBe -245.05
      g.N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("name").head shouldBe "Gray"
    }
    "a Max-step" in {
      g.N.out("balance").hasLabel(DataType.default.`@int`).max.head shouldBe 300
      g.N.out("balance").hasLabel(DataType.default.`@double`).max.head shouldBe 2230.30
      g.N.out("balance").hasLabel(DataType.default.`@number`).max.head shouldBe 2230.30
      g.N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").count().head shouldBe 1
      g.N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").out("name").head shouldBe "Gray"
    }
    "a Min-step" in {
      g.N.out("balance").hasLabel(DataType.default.`@double`).min.head shouldBe -245.05
      g.N.out("balance").hasLabel(DataType.default.`@double`).min.in("balance").out("name").head shouldBe "Levi"
    }
    "a Sum-step" in {
      g.N.out("balance").hasLabel(DataType.default.`@double`).sum.head shouldBe 2496.09
      //      val maxBalanceAll = g.N.out("balance").hasLabel(graph.intType, graph.doubleType, graph.longType).sum().head
      //      maxBalanceAll shouldBe 2796.09
    }

    "a Mean-step" in {
      g.N.out("balance").hasLabel(DataType.default.`@double`).mean.head shouldBe 624.0225
    }

    "a Count-step" in {
      g.N.hasLabel(SampleGraph.Person).count().head shouldBe 6
      g.N
        .hasLabel(Ontology("https://schema.org/Person"))
        .where(_.out(Property("https://schema.org/knows")).count.is(P.gt(1)))
        .count
        .head shouldBe 5
      g.N
        .hasLabel(Ontology("https://schema.org/Person"))
        .where(_.out(Property("https://schema.org/knows")).count.is(P.lt(2)))
        .count
        .head shouldBe 1
    }

    "a Repeat-step" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), max = 3)
        .dedup()
        .out("name")
        .toList
        .size shouldBe 4
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true)
        .dedup()
        .out("name")
        .toList
        .size shouldBe 6
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), _.hasIri(sampleGraph.iri + "/person/345"), 3)
        .dedup()
        .out("name")
        .toList
        .size shouldBe 2
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), _.hasIri(sampleGraph.iri + "/person/345"), 3, true)
        .dedup()
        .out("name")
        .toList
        .size shouldBe 3
    }

    "an Id-step" in {
      val id = g.N.id.head
      id == 0l shouldBe false
    }

    "A TimeLimit-step" ignore {}

    "A limit-step" in {
      g.N.count.head should be > 2l
      g.N.limit(2).count.head shouldNot be > 2l
    }

    "A Tail-step" in {
      val total = g.N.count.head
      g.N.tail(2).count.head should be < total
    }

    "An As-step" in {
      import shapeless._
      import syntax.singleton._
      implicit def stringToF[S <: String](label: S) = () => Witness(label).value
      g.N.hasLabel(ontologies.person).as("a").out(properties.knows).as("b").toList
    }

    "A Select-step" in {
      import shapeless._
      import syntax.singleton._
      implicit def stringToF(label: String) = () => Witness(label).value
      val x = g.V
        .hasLabel(`@int`)
        .as("aname")
        .min
        .in(properties.rate)
        .hasLabel(ontologies.person)
        .as("b")

      val tp2s: Tuple2Type[Int, Node] = x.select(_.a.b).et.asInstanceOf[Tuple2Type[Int, Node]]
//
      val (i1: Int, n1: Node) = x.select.head
////      val (i3: Int, n3: Node)   = x.select(_.a.b).head
      val i5: Int = g.V.hasLabel[Int].toList.head
////      val i4: Int               = x.select(_.a).head
////      val n4: Node              = x.select(_.b).head
      val rint: Int             = x.select("aname").head
      val rnode: Node           = x.select("b").head
      val (i33: Int, n33: Node) = x.select("aname", "b").head
      val (n44: Node, i44: Int) = x.select("b", "aname").head
////
      x.select.head._1 shouldBe 1
////      x.select(_.a.b).head._1 shouldBe 1
////      x.select(_.b.a).head._2 shouldBe 1
////      x.select(_.a.b).head._2.iri shouldBe "person-gray"
////      x.select(_.b.a).head._1.iri shouldBe "person-gray"
//
      x.select("aname", "b").head._2.iri shouldBe sampleGraph.iri + "/person/345"
      x.select("aname").head shouldBe 1
      x.select("b").head.iri shouldBe sampleGraph.iri + "/person/345"
      x.select("b", "aname").head._1.iri shouldBe sampleGraph.iri + "/person/345"
//
      g.V
        .hasLabel(`@int`)
        .as("aname")
        .min
        .in(properties.rate)
        .hasLabel(ontologies.person)
        .as("b")
        .select("b")
        .head
        .iri shouldBe sampleGraph.iri + "/person/345"
    }
  }
}