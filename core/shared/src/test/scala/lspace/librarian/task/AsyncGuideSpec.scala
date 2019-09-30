package lspace.librarian.task

import java.time.LocalDate

import lspace._
import lspace.Label.D._
import lspace.Label.P._
import lspace.datatype.{ListType, NodeURLType}
import lspace.librarian.traversal.Step
import lspace.librarian.traversal.step.{Path, Union}
import lspace.librarian.traversal.util.OutTweaker
import lspace.structure.{GraphFixtures, SampledGraph}
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}
import lspace.types.geo.Point
import lspace.util.SampleGraph
import monix.eval.Task
import monix.reactive.Observable
import shapeless.{HList, HNil}

import scala.concurrent.duration._
import scala.language._

trait AsyncGuideSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  implicit def guide: Guide[Observable]

  val properties = SampleGraph.properties
  val ontologies = SampleGraph.ontologies
  val namespaces = SampleGraph.namespaces

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  def traverse = afterWord("traverse")

  def sampledGraphComputerTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

    "a librarian" can traverse {
      "N.toList(sampleGraph)" in {
        g.N
          .withGraph(sampleGraph)
          .toListF
          .map { nodes =>
            nodes.nonEmpty shouldBe true
            nodes.forall(_.isInstanceOf[Node]) should be(true)
          }
          .timeout(4000.millis)
          .runToFuture
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
        g.N
          .out()
          .withGraph(sampleGraph)
          .toListF
          .map { values =>
            values.nonEmpty shouldBe true
          }
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has("name", P.eqv("Garrison")).out("name")""" in {
        g.N
          .has("name", P.eqv("Garrison"))
          .out("name")
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe "Garrison")
          .timeout(4000.millis)
          .runToFuture
      }
      """N.outMap()""" in {
        g.N.outMap().withGraph(sampleGraph).toListF.map(_.nonEmpty shouldBe true).timeout(4000.millis).runToFuture
      }
      """N.outMap().hasLabel(`@int`)""" in {
//        g.N.outMap().hasLabel(`@int`).withGraph(sampleGraph).toListF.map(_.nonEmpty shouldBe true).runToFuture
        succeed
      }
      """N.has("name", P.eqv("Garrison")).outMap()""" in {
        g.N
          .has("name", P.eqv("Garrison"))
          .outMap()
          .withGraph(sampleGraph)
          .headF
          .map(_.size shouldBe 5)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.outE()""" in {
        g.N
          .outE()
          .withGraph(sampleGraph)
          .toListF
          .map { values =>
            values.nonEmpty shouldBe true
            values.take(1).exists(_.isInstanceOf[Edge[_, _]]) should be(true)
          }
          .timeout(4000.millis)
          .runToFuture
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
//          .out(`@id`)
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
      "N.has(properties.birthDate)" in {
        g.N
          .has(properties.birthDate)
          .count()
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 6)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))""" in {
        g.N
          .has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 2)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))""" in {
        g.N
          .has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 3)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))""" in {
        g.N
          .has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 3)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))""" in {
        g.N
          .has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 4)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
        g.N
          .has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 2)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
        g.N
          .has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 3)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
        g.N
          .has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(t => t shouldBe 3)
          .timeout(4000.millis)
          .runToFuture
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
        g.N
          .has(properties.geo, P.within(Point(72.0403, 60.90879)))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 1)
          .timeout(4000.millis)
          .runToFuture
      }
      """g.N.hasIri(sampleGraph.iri + "/place/123").hasNot(properties.birthDate)""" in {
        g.N
          .hasIri(sampleGraph.iri + "/place/123")
          .hasNot(properties.birthDate)
          .withGraph(sampleGraph)
          .toListF
          .map(_.nonEmpty shouldBe true)
          .runToFuture
      }
      """g.N.hasIri(sampleGraph.iri + "/place/123").hasNot(properties.name)""" in {
        g.N
          .hasIri(sampleGraph.iri + "/place/123")
          .hasNot(properties.name)
          .withGraph(sampleGraph)
          .toListF
          .map(_.isEmpty shouldBe true)
          .runToFuture
      }
      "a HasId-step" in {
        g.N
          .hasIri(sampleGraph.iri + "/place/123")
          .id
          .withGraph(sampleGraph)
          .headF
          .flatMap { someId =>
            g.N
              .hasId(someId)
              .out(`@id`)
              .withGraph(sampleGraph)
              .headF
              .map(_ shouldBe sampleGraph.iri + "/place/123")
          }
          .timeout(4000.millis)
          .runToFuture
      }
      "a HasIri-step" in {
        g.N
          .hasIri(sampleGraph.iri + "/place/123")
          .withGraph(sampleGraph)
          .toListF
          .map(_.nonEmpty shouldBe true)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.coin(0.0)" in {
        g.N.coin(0.0).withGraph(sampleGraph).toListF.map(_.isEmpty shouldBe true).timeout(4000.millis).runToFuture
      }
      "N.coin(1.0)" in {
        g.N.coin(1.0).withGraph(sampleGraph).toListF.map(_.nonEmpty shouldBe true).timeout(4000.millis).runToFuture
      }
      """N.constant("abc")""" in {
        g.N.constant("abc").head.withGraph(sampleGraph).headF.map(_ shouldBe "abc").timeout(4000.millis).runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").out("https://example.org/knows").out("https://example.org/knows").path(_.out("name").head)""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .out("https://example.org/knows")
          .out("https://example.org/knows")
          .path(_.out("name").head)
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set(List(Some("Levi"), Some("Gray"), Some("Kevin")),
                                    List(Some("Levi"), Some("Gray"), Some("Levi")),
                                    List(Some("Levi"), Some("Yoshio"), Some("Levi"))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").out("https://example.org/knows").out("https://example.org/knows").path(_.out("name"))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .out("https://example.org/knows")
          .out("https://example.org/knows")
          .path(_.out("name"))
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set(List(List("Levi"), List("Gray"), List("Kevin")),
                                    List(List("Levi"), List("Gray"), List("Levi")),
                                    List(List("Levi"), List("Yoshio"), List("Levi"))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").out("https://example.org/knows").out("https://example.org/knows").path(_.out("name").count)""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .out("https://example.org/knows")
          .out("https://example.org/knows")
          .path(_.out("name").count)
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set(List(1, 1, 1), List(1, 1, 1), List(1, 1, 1)))
          .timeout(4000.millis)
          .runToFuture
      }
      "N.where(_.has(properties.balance)).out(properties.name)" in {
        g.N
          .where(_.has(properties.balance))
          .out(properties.name)
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set("Yoshio", "Levi", "Gray", "Kevin", "Stan"))
          .timeout(4000.millis)
          .runToFuture
      }
      "N.and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000))).count" in {
        g.N
          .and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000)))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 2)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count" in {
        g.N
          .or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200)))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 3)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count" in {
        g.N
          .union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200)))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 3)
          .timeout(4000.millis)
          .runToFuture

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
          .withGraph(sampleGraph)
          .toListF
          .map(_ shouldBe List(1, 1, 1, 1, 1, 1))
          .timeout(4000.millis)
          .runToFuture
      }
      "N.coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200))).count" in {
        g.N
          .coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200)))
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 3)
          .timeout(4000.millis)
          .runToFuture
      }
      """g.N.coalesce(_.has(keys.rate, P.gte(4)).constant(1), _.constant(0)).sum.withGraph(graph).head""" in {
        g.N
          .coalesce(_.has(properties.rate, P.gte(4)).constant(1), _.constant(0))
          .sum()
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 2)
          .timeout(4000.millis)
          .runToFuture
      }
      """g.V.coalesce(_.hasLabel[String], _.hasLabel[Int], _.hasLabel[Boolean])""" in {
        g.V
          .coalesce(_.out().hasLabel[String], _.hasLabel[Int], _.out().hasLabel[Boolean])
          .withGraph(sampleGraph)
          .toListF
          .map(_.nonEmpty shouldBe true)
          .timeout(4000.millis)
          .runToFuture
      }
      """g.N.hasIri(sampleGraph.iri + "/person/12345").project(_.coalesce(_.out(properties.name).hasLabel[String],
        |                              _.out(properties.birthDate).hasLabel[LocalDate],
        |                              _.out(properties.rate).hasLabel[Int]))
        |          .by(_.coalesce(_.out(properties.name).hasLabel[Int],
        |                         _.out(properties.birthDate).hasLabel[LocalDate],
        |                         _.out(properties.rate).hasLabel[Int]))
        |          .by(
        |            _.coalesce(_.out(properties.name).hasLabel[Int],
        |                       _.out(properties.birthDate).hasLabel[Int],
        |                       _.out(properties.rate).hasLabel[Int])
        |          )""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .project(_.coalesce(_.out(properties.name).hasLabel[String],
                              _.out(properties.birthDate).hasLabel[LocalDate],
                              _.out(properties.rate).hasLabel[Int]))
          .by(_.coalesce(_.out(properties.name).hasLabel[Int],
                         _.out(properties.birthDate).hasLabel[LocalDate],
                         _.out(properties.rate).hasLabel[Int]))
          .by(
            _.coalesce(_.out(properties.name).hasLabel[Int],
                       _.out(properties.birthDate).hasLabel[Int],
                       _.out(properties.rate).hasLabel[Int])
          )
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe (List("Levi"), List(LocalDate.parse("2008-12-20")), List(2)))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/place/123").choose(_.count.is(P.eqv(1)), _.constant(true), _.constant(false))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/place/123")
          .choose(_.count.is(P.eqv(1)), _.constant(true), _.constant(false))
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe true)
          .timeout(4000.millis)
          .runToFuture
      }
      """.hasIri(sampleGraph.iri + "/place/123").choose(_.count.is(P.eqv(2)), _.constant(true), _.constant(false))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/place/123")
          .choose(_.count.is(P.eqv(2)), _.constant(true), _.constant(false))
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe false)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.not(_.has(`@label`))" in {
        g.N
          .not(_.has(`@label`))
          .withGraph(sampleGraph)
          .toListF
          .map(_.nonEmpty shouldBe true)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").group(_.label).mapValues(_.project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0))))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .group(_.label())
          .mapValues(_.project(_.out(properties.name)).by(_.out(properties.balance).hasLabel[Double].is(P.gt(200.0))))
          .withGraph(sampleGraph)
          .headF
          .map(r =>
            (r: (List[Ontology], List[(List[Any], List[Double])])) shouldBe ((List(ontologies.person),
                                                                              List((List("Levi"), List())))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").group(_.label().dedup).mapValues(_.project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0))))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .group(_.label().dedup)
          .mapValues(_.project(_.out(properties.name)).by(_.out(properties.balance).hasLabel[Double].is(P.gt(200.0))))
          .withGraph(sampleGraph)
          .headF
          .map(r =>
            (r: (Set[Ontology], List[(List[Any], List[Double])])) shouldBe ((Set(ontologies.person),
                                                                             List((List("Levi"), List())))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").group(_.out(properties.knows).count()).mapValues(_.project(_.out(properties.name)).by(_.out(properties.balance).hasLabel[Double].is(P.gt(200.0))))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .group(_.out(properties.knows).count())
          .mapValues(_.project(_.out(properties.name)).by(_.out(properties.balance).hasLabel[Double].is(P.gt(200.0))))
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe ((2, List((List("Levi"), List())))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").group(_.out(properties.knows).head).mapValues(_.head.project(_.out(properties.name)).by(_.out(properties.balance).hasLabel[Double].is(P.gt(200.0)).head))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .group(_.out(properties.knows).hasLabel[Int].head())
          .mapValues(_.head()
            .project(_.out(properties.name))
            .by(_.out(properties.balance).hasLabel[Double].is(P.gt(200.0)).head))
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe ((None, Some((List("Levi"), None)))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").out(properties.knows).project(_.out(properties.name).head).by(_.out(properties.balance).hasLabel[Double].is(P.gt(2001.0)))""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .out(properties.knows)
          .project(_.out(properties.name).head)
          .by(_.out(properties.balance).hasLabel[Double].is(P.gt(2001.0)))
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set((Some("Gray"), List(2230.3)), (Some("Yoshio"), List())))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").project(_.out(properties.knows).project(_.out(properties.name))
        |.by(_.out(properties.balance).hasLabel[Double].is(P.gt(2001.0)))
      """.stripMargin in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .project(_.out(properties.knows)
            .project(_.out(properties.name))
            .by(_.out(properties.balance).hasLabel[Double].is(P.gt(2001.0))))
          .by(_.out(properties.name))
          .withGraph(sampleGraph)
          .headF
          .map {
            case (tuples, name) =>
              (tuples.toSet, name) shouldBe (Set((List("Gray"), List(2230.3)), (List("Yoshio"), List())), List("Levi"))
          }
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").project().by(_.out(properties.knows).count())
      """.stripMargin in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .project()
          .by(_.out(properties.knows).count())
          .withGraph(sampleGraph)
          .headF
          .map {
            case (node: Node, count: Long) =>
              count shouldBe 2
          }
          .timeout(4000.millis)
          .runToFuture
      }
      "N.union(_.has(properties.balance, P.lt(0.0)), _.has(properties.balance, P.gt(2000.0)))" in {
        g.N
          .union(
            _.has(properties.balance, P.lt(0.0)),
            _.has(properties.balance, P.gt(2000.0))
          )
          .dedup()
          .out(properties.name)
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set("Levi", "Gray"))
          .timeout(4000.millis)
          .runToFuture
      }
      "N.group(_.label())" in {
        g.N
          .group(_.label())
          .withGraph(sampleGraph)
          .toListF
          .map { groupedNodes =>
            groupedNodes.size shouldBe 2
            groupedNodes.head._1.size shouldBe 1
            groupedNodes.head._2.size should be > 1
          }
          .timeout(4000.millis)
          .runToFuture
      }
      "N.group(_.label()).head" in {
        g.N
          .group(_.label())
          .head
          .withGraph(sampleGraph)
          .headF
          .map { groupedNodes =>
            succeed
          }
          .timeout(4000.millis)
          .runToFuture
      }
      "N.group(_.label()).mapValues(_.count))" in {
        g.N
          .group(_.label())
          .mapValues(_.count)
          .withGraph(sampleGraph)
          .toMapF
          .map { groupedNodes =>
            groupedNodes.values.toSet shouldBe Set(4l, 6l)
          }
          .timeout(4000.millis)
          .runToFuture
      }
      "N.hasIri(sampleGraph.iri + \"/person/12345\").group(_.label()).mapValues(_.outMap())" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .group(_.label())
          .mapValues(_.outMap())
          .withGraph(sampleGraph)
          .toListF
          .map {
            case List((ontologies, List(maps: Map[Property, List[Any]], _*)), _*) =>
              ontologies.nonEmpty shouldBe true
          }
          .timeout(4000.millis)
          .runToFuture
      }
//      "N.group(_.label()).outMap().outMap()" in {
//        g.N
//          .group(_.label()).mapValues(_.outMap()
//            .outMap())
//          .withGraph(sampleGraph)
//          .toListF
//          .map { doubledoubleGroupedNodes =>
//            doubledoubleGroupedNodes.nonEmpty shouldBe true
//          }
//          .runToFuture
//      }
//      "a Drop-step" ignore {
//        val p         = sampleGraph + Ontology("https://example.org/Person")
//        val weirdname = "lkaskfdmnowenoiafps"
//        p --- "name" --> weirdname
//        g.N.has("name", P.eqv(weirdname)).count.head shouldBe 1
//        g.N.has("name", P.eqv(weirdname)).drop().iterate()
//        g.N.has("name", P.eqv(weirdname)).count.head shouldBe 0
//      }
      "N.limit(1).union(_.out().limit(1), _.out().limit(1))" in {
        g.N
          .limit(1)
          .union(_.out().limit(1), _.out().limit(1))
          .withGraph(sampleGraph)
          .toListF
          .map(_.size shouldBe 2)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.limit(1).union(_.out().limit(1), _.out().limit(1)).dedup()" in {
        g.N
          .limit(1)
          .union(_.out().limit(1), _.out().limit(1))
          .dedup()
          .withGraph(sampleGraph)
          .toListF
          .map(_.size shouldBe 1)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.limit(1).union(_.out().limit(2), _.out().limit(2))" in {
        g.N
          .limit(1)
          .union(_.out().limit(2), _.out().limit(2))
          .withGraph(sampleGraph)
          .toListF
          .map(_.size shouldBe 4)
          .timeout(4000.millis)
          .runToFuture
      }
      "N.limit(1).union(_.out().limit(2), _.out().limit(2)).dedup()" in {
        g.N
          .limit(1)
          .union(_.out().limit(2), _.out().limit(2))
          .dedup()
          .withGraph(sampleGraph)
          .toListF
          .map(_.size shouldBe 2)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/56789").out(properties.knows).skip(1).count""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/56789")
          .out(properties.knows)
          .skip(1)
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 1)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/56789").out(properties.knows).range(1,1).count""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/56789")
          .out(properties.knows)
          .range(1, 1)
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 1)
          .runToFuture
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
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe "Crystal Springs")
          .timeout(4000.millis)
          .runToFuture
      }
      """N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("balance")""" in {
        g.N
          .order(_.out("balance").hasLabel(`@double`), false)
          .limit(1)
          .out("balance")
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 2230.30)
          .timeout(4000.millis)
          .runToFuture
      }
      //      g.N.order(_.out("balance").hasLabel[Double], false).limit(1).out("balance").head shouldBe 2230.30
      """N.order(_.out("balance").hasLabel(`@double`)).limit(1).out("balance")""" in {
        g.N
          .order(_.out("balance").hasLabel(`@double`))
          .limit(1)
          .out("balance")
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe -245.05)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("name")""" in {
        g.N
          .order(_.out("balance").hasLabel(`@double`), false)
          .limit(1)
          .out("name")
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe "Gray")
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@int`).max""" in {
        g.N
          .out("balance")
          .hasLabel(`@int`)
          .max
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 300)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@double`).max""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .max
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 2230.30)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@number`).max""" in {
        g.N
          .out("balance")
          .hasLabel(`@number`)
          .max
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 2230.30)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@double`).max.in("balance").count()""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .max
          .in("balance")
          .count()
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 1)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@double`).max.in("balance").out("name")""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .max
          .in("balance")
          .out("name")
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe "Gray")
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@double`).min""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .min
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe -245.05)
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@double`).min.in("balance").out("name")""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .min
          .in("balance")
          .out("name")
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe "Levi")
          .timeout(4000.millis)
          .runToFuture
      }
      """N.out("balance").hasLabel(`@double`).sum""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .sum
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 2496.09)
          .timeout(4000.millis)
          .runToFuture
        //      val maxBalanceAll = g.N.out("balance").hasLabel(graph.intType, graph.doubleType, graph.longType).sum().head
        //      maxBalanceAll shouldBe 2796.09
      }
//
      """N.out("balance").hasLabel(`@double`).mean""" in {
        g.N
          .out("balance")
          .hasLabel(`@double`)
          .mean
          .withGraph(sampleGraph)
          .headF
          .map(_ shouldBe 624.0225)
          .timeout(4000.millis)
          .runToFuture
      }
//
//      "a Count-step" ignore {
//        g.N.hasLabel(SampleGraph.Person).count().head shouldBe 6
//        g.N
//          .hasLabel(Ontology("https://example.org/Person"))
//          .where(_.out(Property("https://example.org/knows")).count.is(P.gt(1)))
//          .count
//          .head shouldBe 5
//        g.N
//          .hasLabel(Ontology("https://example.org/Person"))
//          .where(_.out(Property("https://example.org/knows")).count.is(P.lt(2)))
//          .count
//          .head shouldBe 1
//      }
//
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://example.org/knows")), max = 2).dedup().out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://example.org/knows")), max = 2)
          .dedup()
          .out("name")
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set("Yoshio", "Gray", "Garrison", "Stan"))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://example.org/knows")), max = 2, noloop = true).dedup().path(_.out("name").hasLabel[String])""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://example.org/knows")), max = 2, noloop = true)
//          .dedup()
          .path(_.out("name").hasLabel[String])
//          .path(_.out("name").hasLabel[String])
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set(List(List("Levi"), List("Gray"), List("Kevin"), List("Garrison")),
                                    List(List("Levi"), List("Gray"), List("Kevin"), List("Stan"))))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://example.org/knows")), max = 3, collect = true).dedup().out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://example.org/knows")), max = 3, collect = true)
          .dedup()
          .out("name")
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set("Yoshio", "Gray", "Garrison", "Stan", "Levi", "Kevin"))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://example.org/knows")), 3)""" +
        """(_.hasIri(sampleGraph.iri + "/person/345")).out("name")""" in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://example.org/knows")), 3)(
            _.out(Property("https://example.org/knows")).hasIri(sampleGraph.iri + "/person/345"))
          .dedup()
          .out("name")
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set("Levi", "Kevin"))
          .timeout(4000.millis)
          .runToFuture
      }
      """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://example.org/knows"), 3, true)""" +
        """(_.hasIri(sampleGraph.iri + "/person/345")).dedup().out("name")""".stripMargin in {
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .repeat(_.out(Property("https://example.org/knows")), 3, true)(_.hasIri(sampleGraph.iri + "/person/345"))
          .dedup()
          .out("name")
          .withGraph(sampleGraph)
          .toListF
          .map(_.toSet shouldBe Set("Gray", "Yoshio", "Levi"))
          .timeout(4000.millis)
          .runToFuture
      }
//
//      "an Id-step" in {
//        val id = g.N.id.head
//        id == 0l shouldBe false
//      }
//
      "g.N.repeat(_.out()).timeLimit(Time.apply(200.millis)).count" in {
        import scala.concurrent.duration._
        g.N
          .repeat(_.out(), collect = true)
          .timeLimit(200l)
          .count
          .withGraph(sampleGraph)
          .headF
          .map(_ should be > 1l)
          .timeout(4000.millis)
          .runToFuture
      }
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
