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
      val (query, graphql)  = decoder.process(""" { name }""".stripMargin)(activeContext)
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
      val (query, graphql)  = decoder.process(""" { name  description }""".stripMargin)(activeContext)
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
        decoder.process(""" { name { description2 } description }""".stripMargin)(activeContext)
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
        decoder.process(""" { name { description2 name2 } description }""".stripMargin)(activeContext)
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
  }
}
