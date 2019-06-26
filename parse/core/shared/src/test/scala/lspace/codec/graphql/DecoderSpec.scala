package lspace.codec.graphql

import lspace._
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.librarian.traversal.step.Out
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.collection.immutable.ListMap
import scala.concurrent.Future

class DecoderSpec extends AsyncWordSpec with Matchers {

  val graph: Graph                          = Graph("lspace.codec.turtle.DecoderSpec")
  val decoder: lspace.codec.graphql.Decoder = lspace.codec.graphql.Decoder()
  import lspace.Implicits.Scheduler.global

  val schemaName         = Property.properties.getOrCreate("http://schema.org/name")
  val schemaName2        = Property.properties.getOrCreate("http://schema.org/name2")
  val schemaDescription  = Property.properties.getOrCreate("http://schema.org/description")
  val schemaDescription2 = Property.properties.getOrCreate("http://schema.org/description2")
  val activeContext = ActiveContext(
//    `@prefix` = ListMap(
//      "name" -> "http://schema.org/name"
//    ),
    definitions = Map(
      "names" -> ActiveProperty(schemaName,
                                `@type` = Label.D.`@string` :: Nil,
                                `@container` = List(lspace.codec.`@container`.`@list`))(),
      "name"         -> ActiveProperty(schemaName, `@type` = Label.D.`@string` :: Nil)(),
      "name2"        -> ActiveProperty(schemaName2, `@type` = Label.D.`@string` :: Nil)(),
      "description"  -> ActiveProperty(schemaDescription, `@type` = Label.D.`@string` :: Nil)(),
      "description2" -> ActiveProperty(schemaDescription2, `@type` = Label.D.`@string` :: Nil)()
    )
  )

  "The GraphQL Decoder" should {
    "parse ' { name }'" in {
      val (query, graphql)  = decoder.findQuery(""" { name }""".stripMargin)(activeContext)
      val expectedTraversal = g.project(_.out(schemaName))
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
    "parse ' { name  description }'" in {
      val (query, graphql)  = decoder.findQuery(""" { name  description }""".stripMargin)(activeContext)
      val expectedTraversal = g.project(_.out(schemaName)).by(_.out(schemaDescription))
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
    "parse ' { name { description2 } description }'" in {
      val (query, graphql) =
        decoder.findQuery(""" { name { description2 } description }""".stripMargin)(activeContext)
      val expectedTraversal = g
        .project(_.out(schemaName).project(_.out(schemaDescription2)))
        .by(_.out(schemaDescription))
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
    "parse ' { name { description2 name2 } description }'" in {
      val (query, graphql) =
        decoder.findQuery(""" { name { description2 name2 } description }""".stripMargin)(activeContext)
      val expectedTraversal = g
        .project(_.out(schemaName).project(_.out(schemaDescription2)).by(_.out(schemaName2)))
        .by(_.out(schemaDescription))
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }

    "parse '{ _:( limit: 1) { name } }'" in {
      val (query, graphql)  = decoder.findQuery(""" { _( limit: 1) { name } }""".stripMargin)(activeContext)
      val expectedTraversal = g.project(_.out(schemaName)).limit(1)
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
    "parse '{ _:( limit: 1, offset: 3) { name } }'" in {
      val (query, graphql)  = decoder.findQuery(""" { _( limit: 1, offset: 3) { name } }""".stripMargin)(activeContext)
      val expectedTraversal = g.project(_.out(schemaName)).range(3, 4)
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
    "parse '{ _:( limit: 1 offset: 3) { name } }'" in {
      val (query, graphql)  = decoder.findQuery(""" { _( limit: 4 offset: 3) { name } }""".stripMargin)(activeContext)
      val expectedTraversal = g.project(_.out(schemaName)).range(3, 7)
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
    "parse '{ _:( name2: \"abc\") { name } }'" in {
      val (query, graphql)  = decoder.findQuery(""" { _( name2: "abc") { name } }""".stripMargin)(activeContext)
      val expectedTraversal = g.has(schemaName2, P.eqv("abc")).project(_.out(schemaName))
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }

    }
    "parse ' { name(name2: \"abc\") { description2 name2 } description }'" in {
      val (query, graphql) =
        decoder.findQuery(""" { name(name2: "abc") { description2 name2 } description }""".stripMargin)(activeContext)
      val expectedTraversal = g
        .project(
          _.has(schemaName2, P.eqv("abc")).out(schemaName).project(_.out(schemaDescription2)).by(_.out(schemaName2)))
        .by(_.out(schemaDescription))
      Future {
        query.toTraversal.stepsList shouldBe expectedTraversal.untyped.steps
        query.toTraversal shouldBe expectedTraversal
        query.toTraversal.untyped shouldBe expectedTraversal.untyped
        query.toTraversal.et shouldBe expectedTraversal.et
        graphql shouldBe ""
      }
    }
  }
}
