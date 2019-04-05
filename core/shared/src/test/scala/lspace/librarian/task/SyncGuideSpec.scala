package lspace.librarian.task

import java.time.LocalDate

import lspace.datatype.DataType
import lspace.datatype.DataType.default.{`@double`, `@int`, `@string`}
import lspace.{g, P}
import lspace.structure._
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}

//trait GuideSpec[F[_]] extends Matchers {
//  implicit def guide: Guide[F]
//
//  val properties = SampleGraph.properties
//  val ontologies = SampleGraph.ontologies
//  val namespaces = SampleGraph.namespaces
//
//  def sampledGraphComputerTests(sampledGraph: SampledGraph) = {
//    val graph = sampledGraph.graph
//    Seq(
//      ("name",
//       g.N
//         .has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))
//         .count
//         .withGraph(graph)
//         .headF,
//       2)
//    )
//  }
//}
trait SyncGuideSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  import lspace.Implicits.Scheduler.global

  implicit def guide: Guide[Stream]

  val properties = SampleGraph.properties
  val ontologies = SampleGraph.ontologies
  val namespaces = SampleGraph.namespaces

  def traverse = afterWord("traverse")

  def sampledGraphComputerTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

//    "N.out()" in {
//      g.N.out().withGraph(sampleGraph)

    """N.outMap()""" in {
      g.N.outMap().withGraph(sampleGraph).toListF.map(_.nonEmpty shouldBe true).task.runToFuture
    }
    """N.outMap().hasLabel(`@int`)""" in {
      g.N.outMap().hasLabel(`@int`).withGraph(sampleGraph).toListF.map(_.nonEmpty shouldBe true).task.runToFuture
    }
    """N.has("name", P.eqv("Garrison")).outMap()""" in {
      g.N.has("name", P.eqv("Garrison")).outMap().withGraph(sampleGraph).headF.map(_.size shouldBe 5).task.runToFuture
    }
    "N.out().hasLabel(ontologies.person)" in {
//      g.N.out().hasLabel(ontologies.person).withGraph(sampleGraph).toListF.task.runToFuture
      (for {
        nodes <- g.N.withGraph(sampleGraph).toListF
      } yield {
        nodes.nonEmpty shouldBe true
        nodes.forall(_.isInstanceOf[Node]) should be(true)
      }).task.runToFuture
    }
    "N.has(properties.birthDate)" in {
      import lspace.librarian.traversal._
      (sampleGraph *> g.N
        .has(properties.birthDate)
        .count()).headF.map(_ shouldBe 6).task.runToFuture
    }
    """N.has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 2)
        .task
        .runToFuture
    }
    """N.has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
        .runToFuture
    }
    """N.has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
        .runToFuture
    }
    """N.has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 4)
        .task
        .runToFuture
    }
    """N.has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
      g.N
        .has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 2)
        .task
        .runToFuture
    }
    """N.has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
      g.N
        .has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
        .runToFuture
    }
    """N.has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
      g.N
        .has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
        .runToFuture
    }
    "N.where(_.has(properties.balance)).out(properties.name)" in {
      g.N
        .where(_.has(properties.balance))
        .out(properties.name)
        .withGraph(sampleGraph)
        .toListF
        .map(_.toSet shouldBe Set("Yoshio", "Levi", "Gray", "Kevin", "Stan"))
        .task
        .runToFuture
    }
    "N.and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000))).count" in {
      g.N
        .and(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(3000)))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 2)
        .task
        .runToFuture

    }
    "N.or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count" in {
      g.N
        .or(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200)))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
        .runToFuture

    }
    "N.union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200))).count" in {
      g.N
        .union(_.has(properties.balance, P.gt(300)), _.has(properties.balance, P.lt(-200)))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
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
        .task
        .runToFuture

    }
    "N.coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200))).count" in {
      g.N
        .coalesce(_.has(properties.rate, P.gte(4)), _.has(properties.balance, P.lt(-200)))
        .count
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 3)
        .task
        .runToFuture

    }
    """N.hasIri(sampleGraph.iri + "/place/123").choose(_.count.is(P.eqv(1)), _.constant(true), _.constant(false))""" in {
      g.N
        .hasIri(sampleGraph.iri + "/place/123")
        .choose(_.count.is(P.eqv(1)), _.constant(true), _.constant(false))
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe true)
        .task
        .runToFuture
    }
    """.hasIri(sampleGraph.iri + "/place/123").choose(_.count.is(P.eqv(2)), _.constant(true), _.constant(false))""" in {
      g.N
        .hasIri(sampleGraph.iri + "/place/123")
        .choose(_.count.is(P.eqv(2)), _.constant(true), _.constant(false))
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe false)
        .task
        .runToFuture
    }
    "N.not(_.has(Property.default.`@label`))" in {
      g.N
        .not(_.has(Property.default.`@label`))
        .withGraph(sampleGraph)
        .toListF
        .map(_.nonEmpty shouldBe true)
        .task
        .runToFuture
    }
    """N.hasIri(sampleGraph.iri + "/person/12345").group(_.label()).project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0)))""" in {
      val x = g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .group(_.label())
        .project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0)))
        .withGraph(sampleGraph)
        .toMap

      Task(x shouldBe Map((List(ontologies.person) -> List((List("Levi"), List()))))).runToFuture

    }
    """N.hasIri(sampleGraph.iri + "/person/12345").group(_.out(properties.knows).count()).project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0)))""" in {
      Task(
        g.N
          .hasIri(sampleGraph.iri + "/person/12345")
          .group(_.out(properties.knows).count())
          .project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0)))
          .withGraph(sampleGraph)
          .head shouldBe ((2, List((List("Levi"), List()))))).runToFuture
    }
//    """N.hasIri(sampleGraph.iri + "/person/12345").group(_.out(properties.knows).count()).project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0)).head)""" in {
//      g.N
//        .hasIri(sampleGraph.iri + "/person/12345")
//        .group(_.out(properties.knows).count())
//        .head
//        .project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(200.0)).head)
//        .withGraph(sampleGraph)
//        .head shouldBe ((2, Some((List("Levi"), None))))
//    }
    """N.hasIri(sampleGraph.iri + "/person/12345").project(_.out(Property.default.`@id`), _.out(Property.default.`@type`))""" in {
      val x: List[(List[Any], List[Double])] = g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .out(properties.knows)
        .project(_.out(properties.name), _.out(properties.balance).hasLabel[Double].is(P.gt(2000.0)))
        .withGraph(sampleGraph)
        .toList

      Task(x.toSet shouldBe Set((List("Gray"), List(2230.3)), (List("Yoshio"), List()))).runToFuture

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
        .task
        .runToFuture

    }
    "N.group(_.label())" in {
      g.N
        .group(_.label())
        .withGraph(sampleGraph)
        .toListF
        .map(_.nonEmpty shouldBe true)
        .task
        .runToFuture

    }
    "N.group(_.label()).outMap()" in {
      g.N
        .group(_.label())
        .outMap()
        .withGraph(sampleGraph)
        .toListF
        .map(_.nonEmpty shouldBe true)
        .task
        .runToFuture

    }
    "N.group(_.label()).outMap().outMap()" in {
      g.N
        .group(_.label())
        .outMap()
        .outMap()
        .withGraph(sampleGraph)
        .toListF
        .map(_.nonEmpty shouldBe true)
        .task
        .runToFuture

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
      g.N
        .limit(1)
        .union(_.out().limit(1), _.out().limit(1))
        .withGraph(sampleGraph)
        .toListF
        .map(_.size shouldBe 2)
        .task
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
        .task
        .runToFuture

    }
    "N.limit(1).union(_.out().limit(2), _.out().limit(2))" in {
      g.N
        .limit(1)
        .union(_.out().limit(2), _.out().limit(2))
        .withGraph(sampleGraph)
        .toListF
        .map(_.size shouldBe 4)
        .task
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
        .task
        .runToFuture

    }
    """N.order(_.out("name").hasLabel(`@string`)).local(_.out("name").limit(1))""" in {
      //      g.N.order(_.out("name").hasLabel[String]).local(_.out("name").limit(1)).head shouldBe "Crystal Springs"
      g.N
        .order(_.out("name").hasLabel(`@string`))
        .local(_.out("name").limit(1))
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe "Crystal Springs")
        .task
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
        .task
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
        .task
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
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@int`).max""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@int`)
        .max
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 300)
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).max""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .max
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 2230.30)
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@number`).max""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@number`)
        .max
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 2230.30)
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").count()""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .max
        .in("balance")
        .count()
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 1)
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").out("name")""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .max
        .in("balance")
        .out("name")
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe "Gray")
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).min""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .min
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe -245.05)
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).min.in("balance").out("name")""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .min
        .in("balance")
        .out("name")
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe "Levi")
        .task
        .runToFuture

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).sum""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .sum
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 2496.09)
        .task
        .runToFuture

      //      val maxBalanceAll = g.N.out("balance").hasLabel(graph.intType, graph.doubleType, graph.longType).sum().head
      //      maxBalanceAll shouldBe 2796.09
    }
    //
    """N.out("balance").hasLabel(DataType.default.`@double`).mean""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .mean
        .withGraph(sampleGraph)
        .headF
        .map(_ shouldBe 624.0225)
        .task
        .runToFuture

    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), max = 2).dedup().out("name")""" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), max = 2)
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toListF
        .map(_.toSet shouldBe Set("Yoshio", "Gray", "Garrison", "Stan"))
        .task
        .runToFuture

    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true).dedup().out("name")""" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true)
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toListF
        .map(_.toSet shouldBe Set("Yoshio", "Gray", "Garrison", "Stan", "Levi", "Kevin"))
        .task
        .runToFuture

    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), 3)""" +
      """(_.hasIri(sampleGraph.iri + "/person/345")).out("name")""" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), 3)(
          _.out(Property("https://schema.org/knows")).hasIri(sampleGraph.iri + "/person/345"))
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toListF
        .map(_.toSet shouldBe Set("Levi", "Kevin"))
        .task
        .runToFuture

    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows"), 3, true)""" +
      """(_.hasIri(sampleGraph.iri + "/person/345")).dedup().out("name")""".stripMargin in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), 3, true)(_.hasIri(sampleGraph.iri + "/person/345"))
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toListF
        .map(_.toSet shouldBe Set("Gray", "Yoshio", "Levi"))
        .task
        .runToFuture

    }
  }
}
