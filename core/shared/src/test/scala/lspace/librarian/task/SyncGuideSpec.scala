package lspace.librarian.task

import java.time.LocalDate

import lspace.datatype.DataType
import lspace.datatype.DataType.default.{`@double`, `@string`}
import lspace.{g, P}
import lspace.structure.{GraphFixtures, Node, Property, SampledGraph}
import lspace.util.SampleGraph
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

trait SyncGuideSpec extends WordSpec with Matchers with BeforeAndAfterAll with GraphFixtures {

  implicit def guide: Guide[Stream]

  val properties = SampleGraph.properties
  val ontologies = SampleGraph.ontologies
  val namespaces = SampleGraph.namespaces

  def traverse = afterWord("traverse")

  def sampledGraphComputerTests(sampledGraph: SampledGraph) = {
    val sampleGraph = sampledGraph.graph

    "N.out().hasLabel(ontologies.person)" in {
      g.N.out().hasLabel(ontologies.person).withGraph(sampleGraph).toList
      val nodes = g.N.withGraph(sampleGraph).toList
      nodes.nonEmpty shouldBe true
      nodes.forall(_.isInstanceOf[Node]) should be(true)
    }
    "N.has(properties.birthDate)" in {
      import lspace.librarian.traversal._
      (sampleGraph *> g.N
        .has(properties.birthDate)
        .count()).head shouldBe 6
    }
    """N.has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.gt(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 2
    }
    """N.has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.gte(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 3
    }
    """N.has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.lt(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 3
    }
    """N.has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))""" in {
      g.N
        .has(properties.birthDate, P.lte(LocalDate.parse("2002-06-13")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 4
    }
    """N.has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
      g.N
        .has(properties.birthDate, P.inside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 2
    }
    """N.has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
      g.N
        .has(properties.birthDate, P.outside(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 3
    }
    """N.has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))""" in {
      g.N
        .has(properties.birthDate, P.between(LocalDate.parse("2002-06-13"), LocalDate.parse("2009-04-10")))
        .count
        .withGraph(sampleGraph)
        .head shouldBe 3
    }
    """N.order(_.out("name").hasLabel(`@string`)).local(_.out("name").limit(1))""" in {
      //      g.N.order(_.out("name").hasLabel[String]).local(_.out("name").limit(1)).head shouldBe "Crystal Springs"
      g.N
        .order(_.out("name").hasLabel(`@string`))
        .local(_.out("name").limit(1))
        .withGraph(sampleGraph)
        .head shouldBe "Crystal Springs"

    }
    """N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("balance")""" in {
      g.N
        .order(_.out("balance").hasLabel(`@double`), false)
        .limit(1)
        .out("balance")
        .withGraph(sampleGraph)
        .head shouldBe 2230.30

    }
    //      g.N.order(_.out("balance").hasLabel[Double], false).limit(1).out("balance").head shouldBe 2230.30
    """N.order(_.out("balance").hasLabel(`@double`)).limit(1).out("balance")""" in {
      g.N
        .order(_.out("balance").hasLabel(`@double`))
        .limit(1)
        .out("balance")
        .withGraph(sampleGraph)
        .head shouldBe -245.05

    }
    """N.order(_.out("balance").hasLabel(`@double`), false).limit(1).out("name")""" in {
      g.N
        .order(_.out("balance").hasLabel(`@double`), false)
        .limit(1)
        .out("name")
        .withGraph(sampleGraph)
        .head shouldBe "Gray"

    }
    """N.out("balance").hasLabel(DataType.default.`@int`).max""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@int`)
        .max()
        .withGraph(sampleGraph)
        .head shouldBe 300

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).max""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .max
        .withGraph(sampleGraph)
        .head shouldBe 2230.30

    }
    """N.out("balance").hasLabel(DataType.default.`@number`).max""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@number`)
        .max
        .withGraph(sampleGraph)
        .head shouldBe 2230.30

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").count()""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .max
        .in("balance")
        .count()
        .withGraph(sampleGraph)
        .head shouldBe 1

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).max.in("balance").out("name")""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .max
        .in("balance")
        .out("name")
        .withGraph(sampleGraph)
        .head shouldBe "Gray"

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).min""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .min
        .withGraph(sampleGraph)
        .head shouldBe -245.05

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).min.in("balance").out("name")""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .min
        .in("balance")
        .out("name")
        .withGraph(sampleGraph)
        .head shouldBe "Levi"

    }
    """N.out("balance").hasLabel(DataType.default.`@double`).sum""" in {
      g.N
        .out("balance")
        .hasLabel(DataType.default.`@double`)
        .sum
        .withGraph(sampleGraph)
        .head shouldBe 2496.09

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
        .head shouldBe 624.0225

    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), max = 2).dedup().out("name")""" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), max = 2)
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toList
        .toSet shouldBe Set("Yoshio", "Gray", "Garrison", "Stan")
    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true).dedup().out("name")""" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), max = 3, collect = true)
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toList
        .toSet shouldBe Set("Yoshio", "Gray", "Garrison", "Stan", "Levi", "Kevin")
    }
    """N.hasIri(sampleGraph.iri + "/person/12345").repeat(_.out(Property("https://schema.org/knows")), 3)(_.hasIri(sampleGraph.iri + "/person/345")).out("name")""" in {
      g.N
        .hasIri(sampleGraph.iri + "/person/12345")
        .repeat(_.out(Property("https://schema.org/knows")), 3)(
          _.out(Property("https://schema.org/knows")).hasIri(sampleGraph.iri + "/person/345"))
        .dedup()
        .out("name")
        .withGraph(sampleGraph)
        .toList
        .toSet shouldBe Set("Levi", "Kevin")
    }
  }
}
