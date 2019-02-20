package lspace.librarian.task

import java.time.LocalDate

import lspace._
import lspace.datatype._
import lspace.datatype.DataType.default._
import lspace.librarian.traversal.Librarian
import lspace.structure.Property.default._
import org.scalatest.{Assertion, AsyncWordSpec, BeforeAndAfterAll, Matchers}
import lspace.structure._
import lspace.types.vector.Point
import lspace.util.SampleGraph

import scala.concurrent.Future
import scala.language._

trait TaskSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  implicit def guide: Guide

  val properties = SampleGraph.properties
  val ontologies = SampleGraph.ontologies
  val namespaces = SampleGraph.namespaces

  def traverse = afterWord("traverse")

  def sampledGraphComputerTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

    "a librarian" can traverse {
      "N" in {
        g.N.toList(sampleGraph) map { nodes =>
          nodes.nonEmpty shouldBe true
          nodes.forall(_.isInstanceOf[Node]) should be(true)
        }
      }
//      "an E-step" in {
//        val edge = g.E.toList //implicit WithTraversalStream not resolved by IntelliJ IDEA, toList not recognized
//        //      edge.head.id shouldBe 3l
//        //      val edges = g.E(edge).toList
//        //      edges.size shouldBe 1
//        //      edges.head.id shouldBe edge.id
//      }
//      "a V-step" in {
//        val value = g.V.head
//        //      val values = g.V(value).toList
//        //      values.head shouldBe value
//      }
//      "a R-step" in {
//        //      val resource = g.R.head
//        //      val resources = g.R(resource).toList
//        //      resources.head.id shouldBe resource.id
//      }
      "a N.out()" in {
        g.N.out().toList(sampleGraph).map { values =>
          values.nonEmpty shouldBe true
        }
      }
      """N.has("name", P.eqv("Garrison")).out("name")""" in {
        g.N.has("name", P.eqv("Garrison")).out("name").head(sampleGraph).map(_ shouldBe "Garrison")
      }
      """N.outMap()""" in {
        g.N.outMap().toList(sampleGraph).map(_.nonEmpty shouldBe true)
      }
      """N.outMap().hasLabel(`@int`)""" in {
        g.N.outMap().hasLabel(`@int`).toList(sampleGraph).map(_.nonEmpty shouldBe true)
      }
      """N.has("name", P.eqv("Garrison")).outMap()""" in {
        g.N.has("name", P.eqv("Garrison")).outMap().head(sampleGraph).map(_.size shouldBe 5)
      }
      """N.outE()""" in {
        g.N.outE().toList(sampleGraph).map { values =>
          values.nonEmpty shouldBe true
          values.take(1).exists(_.isInstanceOf[Edge[_, _]]) should be(true)
        }
      }
//        g.N.has("name").outE("name").head.key.iri shouldBe "name"
//      "a OutEMap-step" in {
//        g.N.outEMap().toObservable(sampleGraph).nonEmptyL.runToFuture.map(_ shouldBe true)
//      }
//      "a In-step" in {
//        g.V("Garrison").toList(sampleGraph).nonEmpty should be(true)
//        g.V.is(P.eqv("Garrison")).toList(sampleGraph).nonEmpty should be(true)
//        g.V
//          .is(P.eqv("Garrison"))
//          .in("name")
//          .out(Property.default.`@id`)
//          .head shouldBe (sampleGraph.iri + "/person/56789")
//      }
//      "a InMap-step" in {
//        val values = g.N.inMap().toStream
//        values.head.isInstanceOf[Map[Property, Any]] shouldBe true
//        values.nonEmpty shouldBe true
//      }
//      "a InE-step" in {
//        val values = g.N.inE().toStream
//        values.nonEmpty shouldBe true
//        values.take(1).exists(_.isInstanceOf[Edge[_, _]]) should be(true)
//        g.V.is(P.eqv("Garrison")).inE("name").head.key.iri shouldBe "name"
//      }
//      "a InEMap-step" in {
//        g.N.inEMap().toStream.nonEmpty shouldBe true
//      }
      "N.has(properties.birthDate)" in { sampleGraph *> g.N.has(properties.birthDate).count().head(t => t shouldBe 6) }
      """N.has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))
          .count
          .head(t => t shouldBe 2)
      }
      """N.has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))
          .count
          .head(t => t shouldBe 3)
      }
      """N.has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))
          .count
          .head(t => t shouldBe 3)
      }
      """N.has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))
          .count
          .head(t => t shouldBe 4)
      }
      """N.has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .head(t => t shouldBe 2)
      }
      """N.has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .head(t => t shouldBe 3)
      }
      """N.has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
        sampleGraph *> g.N
          .has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .head(t => t shouldBe 3)
      }

//        "numeric predicate" in {
//          g.N.has(properties.balance).count.head shouldBe 5
//          g.N.has(properties.balance, P.gt(300)).count.head shouldBe 2
//          g.N.has(properties.balance, P.gt(300.0)).count.head shouldBe 2
//          g.N.has(properties.balance, P.gte(300)).count.head shouldBe 3
//          g.N.has(properties.balance, P.lt(300)).count.head shouldBe 2
//          g.N.has(properties.balance, P.lte(300)).count.head shouldBe 3
//          g.N.has(properties.balance, P.inside(300, 3000)).count.head shouldBe 2
//          g.N.has(properties.balance, P.inside(300, 3000.5)).count.head shouldBe 2
//          g.N.has(properties.balance, P.outside(300, 3000)).count.head shouldBe 2
//          g.N.has(properties.balance, P.between(300, 3000)).count.head shouldBe 3
//        }
//
      "geometric predicate" in {
        g.N.has(properties.geo, P.within(Point(72.0403, 60.90879))).count.head(sampleGraph).map(_ shouldBe 1)
      }
      "N.hasNot(Property.default.`@label`)" in {
        g.N.hasNot(Property.default.`@label`).toList(sampleGraph).map(_.nonEmpty shouldBe true)
      }
      "a HasId-step" in {
        g.N.hasIri(sampleGraph.iri + "/place/123").id.head(sampleGraph).flatMap { someId =>
          g.N.hasId(someId).out(Property.default.`@id`).head(sampleGraph).map(_ shouldBe sampleGraph.iri + "/place/123")
        }
      }
      "a HasIri-step" in {
        g.N.hasIri(sampleGraph.iri + "/place/123").toList(sampleGraph).map(_.nonEmpty shouldBe true)
      }
      "N.coin(0.0)" in {
        g.N.coin(0.0).toList(sampleGraph).map(_.isEmpty shouldBe true)
      }
      "N.coin(1.0)" in {
        g.N.coin(1.0).toList(sampleGraph).map(_.nonEmpty shouldBe true)
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").out("https://schema.org/knows").out("https://schema.org/knows").path(_.out("name"))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .out("https://schema.org/knows")
          .out("https://schema.org/knows")
          .path(_.out("name"))
          .head(sampleGraph)
          .map(_.size shouldBe 3)
      }
      "N.where(_.has(properties.balance)).out(properties.name)" in {
        g.N.where(_.has(properties.balance)).out(properties.name).toList(sampleGraph).map(_.nonEmpty shouldBe true)
      }
      "N.and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000))).count" in {
        g.N
          .and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000)))
          .count
          .head(sampleGraph)
          .map(_ shouldBe 2)
      }
      "N.or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count" in {
        g.N
          .or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200)))
          .count
          .head(sampleGraph)
          .map(_ shouldBe 3)
      }
      "N.union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count" in {
        g.N
          .union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200)))
          .count
          .head(sampleGraph)
          .map(_ shouldBe 3)

        //      Traversal.WithTraversalStream(g.V.hasLabel(listType[Double])).toList
        //      g.N.out().hasLabel(listType[Double]).toList
        //      g.N.out().hasLabel(listType[Double], listType[Int]).toList.head
        //      Traversal.WithTraversalStream(g.N.out().hasLabel(listType[Double], listType[Int])).toList
        //      g.N.out().hasLabel(listType(), vectorType()).toList.head
        //      g.N.out().hasLabel(intType, doubleType, intType, doubleType, intType, doubleType).et
        //      g.N.out().hasLabel(intType, doubleType, dateTimeType).et
        //      g.N.out().hasLabel(geopointType, dateTimeType, doubleType).et
      }
      "N.hasLabel(ontologies.person).local(_.out(properties.name).count)" in {
        //      g.N.hasLabel(ontologies.person).local(_.out(properties.knows).count).toList shouldBe List(1, 3, 2, 2, 2, 2)
        g.N
          .hasLabel(ontologies.person)
          .local(_.out(properties.name).count)
          .toList(sampleGraph)
          .map(_ shouldBe List(1, 1, 1, 1, 1, 1))
      }
      "N.coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200))).count" in {
        g.N
          .coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200)))
          .count
          .head(sampleGraph)
          .map(_ shouldBe 3)
      }
      "N.not(_.has(Property.default.`@label`))" in {
        g.N
          .not(_.has(Property.default.`@label`))
          .toList(sampleGraph)
          .map(_.nonEmpty shouldBe true)
      }
      "N.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`))" in {
        g.N
          .project(_.out(Property.default.`@id`), _.out(Property.default.`@type`))
          .toList(sampleGraph)
          .map(_.nonEmpty shouldBe true)
      }
      "N.union(_.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`)),_.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`)))" in {
        g.N
          .union(
            _.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`)),
            _.project(_.out(Property.default.`@id`), _.out(Property.default.`@type`))
          )
          .toList(sampleGraph)
          .map(_.nonEmpty shouldBe true)
      }
      "N.group(_.label())" in {
        g.N.group(_.label()).toList(sampleGraph).map { groupedNodes =>
          groupedNodes.nonEmpty shouldBe true
        }
      }
      "N.group(_.label()).outMap()" in {
        g.N.group(_.label()).outMap().toList(sampleGraph).map { doubleGroupedNodes =>
          doubleGroupedNodes.nonEmpty shouldBe true
        }
      }
      "N.group(_.label()).outMap().outMap()" in {
        g.N.group(_.label()).outMap().outMap().toList(sampleGraph).map { doubledoubleGroupedNodes =>
          doubledoubleGroupedNodes.nonEmpty shouldBe true
        }
      }
//      "a Drop-step" ignore {
//        val p         = sampleGraph + Ontology("https://schema.org/Person")
//        val weirdname = "lkaskfdmnowenoiafps"
//        p --- "name" --> weirdname
//        g.N.has("name", P.eqv(weirdname)).count.head shouldBe 1
//        g.N.has("name", P.eqv(weirdname)).drop().iterate()
//        g.N.has("name", P.eqv(weirdname)).count.head shouldBe 0
//      }
      "N.limit(1).union(_.out().limit(1), _.out().limit(1))" in {
        g.N.limit(1).union(_.out().limit(1), _.out().limit(1)).toList(sampleGraph).map(_.size shouldBe 2)
      }
      "N.limit(1).union(_.out().limit(1), _.out().limit(1)).dedup()" in {
        g.N.limit(1).union(_.out().limit(1), _.out().limit(1)).dedup().toList(sampleGraph).map(_.size shouldBe 1)
      }
      "N.limit(1).union(_.out().limit(2), _.out().limit(2))" in {
        g.N.limit(1).union(_.out().limit(2), _.out().limit(2)).toList(sampleGraph).map(_.size shouldBe 4)
      }
      "N.limit(1).union(_.out().limit(2), _.out().limit(2)).dedup()" in {
        g.N.limit(1).union(_.out().limit(2), _.out().limit(2)).dedup().toList(sampleGraph).map(_.size shouldBe 2)
      }

//      "a Is-step" which {
//        "has an equivalent node-type value" in {
//          val levi = g.N.hasIri(sampleGraph.iri + "/person/12345").head
//          g.N.is(P.eqv(levi)).toList.size shouldBe 1
//        }
//        "has an equal value-type value" in {
//          g.V.is(P.eqv(sampleGraph.iri + "/place/123")).toList.size shouldBe 1
//        }
//        "has a greater than value-type value" in {
//          g.V.is(P.contains("place/1234")).toList.size shouldBe 1
//          g.V.is(P.prefix(sampleGraph.iri + "/place/1234")).toList.size shouldBe 1
//          g.V.is(P.suffix("ace/12345")).toList.size shouldBe 1
//        }
//      }
//      "a HasLabel-step" in {
//        g.N.hasLabel(ontologies.place).toList.size should be > 0
//        g.E.hasLabel(ontologies.place).toList.size shouldBe 0
//        g.N.hasLabel(properties.knows).toList.size shouldBe 0
//        g.E.hasLabel(properties.knows).toList.size should be > 0
//        g.V.hasLabel(`@int`).toList.size shouldBe 4
//        g.V.hasLabel(`@double`).toList.size shouldBe 4
//        g.V.hasLabel(`@string`).toList.size shouldBe 19
//        g.V.hasLabel(`@int`, `@double`).toList.size shouldBe 8
//        g.V.hasLabel(`@int`, `@double`, `@string`).toList.size shouldBe 27
//        g.V.hasLabel[Int].toList.size shouldBe 4
//        g.V.hasLabel("@int").toList.size shouldBe 4
//      }
      """N.order(_.out("name").hasLabel(`@string`)).local(_.out("name").limit(1))""" in {
        //      g.N.order(_.out("name").hasLabel[String]).local(_.out("name").limit(1)).head shouldBe "Crystal Springs"
        g.N
          .order(_.out("name").hasLabel(`@string`))
          .local(_.out("name").limit(1))
          .head(sampleGraph)
          .map(_ shouldBe "Crystal Springs")
      }
      """N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("balance")""" in {
        g.N
          .order(_.out("balance").hasLabel(`@double`), false)
          .limit(1)
          .out("balance")
          .head(sampleGraph)
          .map(_ shouldBe 2230.30)
      }
      //      g.N.order(_.out("balance").hasLabel[Double], false).limit(1).out("balance").head shouldBe 2230.30
      """N.order(_.out("balance").hasLabel(`@double`)).limit(1).out("balance")""" in {
        g.N
          .order(_.out("balance").hasLabel(`@double`))
          .limit(1)
          .out("balance")
          .head(sampleGraph)
          .map(_ shouldBe -245.05)
      }
      """N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("name")""" in {
        g.N
          .order(_.out("balance").hasLabel(`@double`), false)
          .limit(1)
          .out("name")
          .head(sampleGraph)
          .map(_ shouldBe "Gray")
      }
      """N.out("balance").hasLabel(DataType.default.`@int`).max""" in {
        g.N.out("balance").hasLabel(DataType.default.`@int`).max.head(sampleGraph).map(_ shouldBe 300)
      }
      """N.out("balance").hasLabel(DataType.default.`@double`).max""" in {
        g.N.out("balance").hasLabel(DataType.default.`@double`).max.head(sampleGraph).map(_ shouldBe 2230.30)
      }
      """N.out("balance").hasLabel(DataType.default.`@number`).max""" in {
        g.N.out("balance").hasLabel(DataType.default.`@number`).max.head(sampleGraph).map(_ shouldBe 2230.30)
      }
      """N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").count()""" in {
        g.N
          .out("balance")
          .hasLabel(DataType.default.`@double`)
          .max
          .in("balance")
          .count()
          .head(sampleGraph)
          .map(_ shouldBe 1)
      }
      """N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").out("name")""" in {
        g.N
          .out("balance")
          .hasLabel(DataType.default.`@double`)
          .max
          .in("balance")
          .out("name")
          .head(sampleGraph)
          .map(_ shouldBe "Gray")
      }
      """N.out("balance").hasLabel(DataType.default.`@double`).min""" in {
        g.N
          .out("balance")
          .hasLabel(DataType.default.`@double`)
          .min
          .head(sampleGraph)
          .map(_ shouldBe -245.05)
      }
      """N.out("balance").hasLabel(DataType.default.`@double`).min.in("balance").out("name")""" in {
        g.N
          .out("balance")
          .hasLabel(DataType.default.`@double`)
          .min
          .in("balance")
          .out("name")
          .head(sampleGraph)
          .map(_ shouldBe "Levi")
      }
      """N.out("balance").hasLabel(DataType.default.`@double`).sum""" in {
        g.N
          .out("balance")
          .hasLabel(DataType.default.`@double`)
          .sum
          .head(sampleGraph)
          .map(_ shouldBe 2496.09)
        //      val maxBalanceAll = g.N.out("balance").hasLabel(graph.intType, graph.doubleType, graph.longType).sum().head
        //      maxBalanceAll shouldBe 2796.09
      }
//
      """N.out("balance").hasLabel(DataType.default.`@double`).mean""" in {
        g.N
          .out("balance")
          .hasLabel(DataType.default.`@double`)
          .mean
          .head(sampleGraph)
          .map(_ shouldBe 624.0225)
      }
//
//      "a Count-step" in {
//        g.N.hasLabel(SampleGraph.Person).count().head shouldBe 6
//        g.N
//          .hasLabel(Ontology("https://schema.org/Person"))
//          .where(_.out(Property("https://schema.org/knows")).count.is(P.gt(1)))
//          .count
//          .head shouldBe 5
//        g.N
//          .hasLabel(Ontology("https://schema.org/Person"))
//          .where(_.out(Property("https://schema.org/knows")).count.is(P.lt(2)))
//          .count
//          .head shouldBe 1
//      }
//
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), max = 3).dedup().out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://schema.org/knows")), max = 3)
          .dedup()
          .out("name")
          .toList(sampleGraph)
          .map(_.size shouldBe 4)
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true).dedup().out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true)
          .dedup()
          .out("name")
          .toList(sampleGraph)
          .map(_.size shouldBe 6)
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), _.hasIri(sampleGraph.iri + "/person/345"), 3).out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://schema.org/knows")), _.hasIri(sampleGraph.iri + "/person/345"), 3)
          .dedup()
          .out("name")
          .toList(sampleGraph)
          .map(_.size shouldBe 2)
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), _.hasIri(sampleGraph.iri + "/person/345"), 3, true).dedup().out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://schema.org/knows")), _.hasIri(sampleGraph.iri + "/person/345"), 3, true)
          .dedup()
          .out("name")
          .toList(sampleGraph)
          .map(_.size shouldBe 3)
      }
//
//      "an Id-step" in {
//        val id = g.N.id.head
//        id == 0l shouldBe false
//      }
//
////      "A TimeLimit-step" ignore {}
//
//      "A limit-step" in {
//        g.N.count.head should be > 2l
//        g.N.limit(2).count.head shouldNot be > 2l
//      }
//
//      "A Tail-step" in {
//        val total = g.N.count.head
//        g.N.tail(2).count.head should be < total
//      }
//
////      "An As-step" in {
////        import shapeless._
////        import syntax.singleton._
////        implicit def stringToF[S <: String](label: S) = () => Witness(label).value
////
////        g.N.hasLabel(ontologies.person).as("a").out(properties.knows).as("b").toList
////      }
//
//      "A Select-step" in {
//        import shapeless._
//        import syntax.singleton._
//        implicit def stringToF(label: String) = () => Witness(label).value
//
//        val x = g.V
//          .hasLabel(`@int`)
//          .as("aname")
//          .min
//          .in(properties.rate)
//          .hasLabel(ontologies.person)
//          .as("b")
//
//        val tp2s: Tuple2Type[Int, Node] = x.select(_.a.b).et.asInstanceOf[Tuple2Type[Int, Node]]
//        //
//        val (i1: Int, n1: Node) = x.select.head
//        ////      val (i3: Int, n3: Node)   = x.select(_.a.b).head
//        val i5: Int = g.V.hasLabel[Int].toList.head
//        ////      val i4: Int               = x.select(_.a).head
//        ////      val n4: Node              = x.select(_.b).head
//        val rint: Int             = x.select("aname").head
//        val rnode: Node           = x.select("b").head
//        val (i33: Int, n33: Node) = x.select("aname", "b").head
//        val (n44: Node, i44: Int) = x.select("b", "aname").head
//        ////
//        x.select.head._1 shouldBe 1
//        ////      x.select(_.a.b).head._1 shouldBe 1
//        ////      x.select(_.b.a).head._2 shouldBe 1
//        ////      x.select(_.a.b).head._2.iri shouldBe "person-gray"
//        ////      x.select(_.b.a).head._1.iri shouldBe "person-gray"
//        //
//        x.select("aname", "b").head._2.iri shouldBe sampleGraph.iri + "/person/345"
//        x.select("aname").head shouldBe 1
//        x.select("b").head.iri shouldBe sampleGraph.iri + "/person/345"
//        x.select("b", "aname").head._1.iri shouldBe sampleGraph.iri + "/person/345"
//        //
//        g.V
//          .hasLabel(`@int`)
//          .as("aname")
//          .min
//          .in(properties.rate)
//          .hasLabel(ontologies.person)
//          .as("b")
//          .select("b")
//          .head
//          .iri shouldBe sampleGraph.iri + "/person/345"
//      }
    }
  }
}
