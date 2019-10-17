package lspace.codec.json.jsonld

import lspace.Label.D._
import lspace._
import lspace.codec.{ActiveContext, ActiveProperty, NamedActiveContext}
import lspace.provider.mem.MemGraph
import lspace.structure.SampledGraph
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.FutureOutcome
import scribe.Level
import scribe.format.Formatter

import scala.collection.immutable.ListMap
import scala.concurrent.Future

abstract class JsonLDEncoderSpec[Json](encoder: JsonLDEncoder[Json]) extends AsyncWordSpec with Matchers {

  import encoder.baseEncoder._
  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Trace), formatter = Formatter.enhanced)
    .replace()

  val graph: Graph                    = MemGraph("DecoderSpec")
  val sampleGraph                     = SampledGraph(MemGraph("DecoderSpec-sample"))
  def createGraph(iri: String): Graph = MemGraph("DecoderSpec-" + iri)

  val initTask = (for {
    sample <- sampleGraph.load
  } yield sample).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }

  "The JsonLDEncoder" can {
    "encode a Node" which {
      "uses a default context if provided" in {
        val person = Ontology.ontologies.getOrCreate("https://example.org/Person")
        val defaultContext = ActiveContext(
          `@prefix` = ListMap("naam" -> "name"),
          definitions = Map(
            "name"    -> ActiveProperty(`@type` = `@string` :: Nil, property = Property("name"))(),
            "nameFor" -> ActiveProperty(`@type` = person :: Nil, `@reverse` = true, property = Property("name"))()
          )
        )
//        println(
//          encoder
//            .fromActiveContext(ActiveContext(
//              `@prefix` = ListMap("naam" -> "http://schema.org/name"),
//              definitions = Map(
//                "http://schema.org/name" -> ActiveProperty(`@type` = `@string` :: Nil,
//                                                           property = Property("http://schema.org/name")),
//                "nameFor" -> ActiveProperty(`@type` = person :: Nil,
//                                            `@reverse` = true,
//                                            property = Property("http://schema.org/name"))
//              )
//            ))
//            .map(_.noSpaces))
        (for {
          sample <- initTask
          stan = sample.persons.Stan.person
          joip = encoder.fromNode(stan)(defaultContext)
          json = joip.json
          ac   = joip.activeContext
          _ = encoder.fromActiveContext(ac).map(_.noSpaces) shouldBe Some(
            """{"naam":{"@id":"name","@type":"@string"},"1":"https://example.org/","nameFor":{"@reverse":"name","@type":"https://example.org/Person"}}""")
          //        _ = println(joip.withContext.noSpaces)
        } yield succeed).runToFuture
      }
    }
    "encode an ActiveContext" which {
      "exists of just a remote context" in {
        val defaultContext =
          ActiveContext(remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext())))
        Future {
          encoder
            .fromActiveContext(defaultContext)
            .map(_.noSpaces)
            .map(_ shouldBe """"https://remote.example.org"""")
            .getOrElse(fail)
        }
      }
      "exists of a remote context and a local context" in {
        val defaultContext =
          ActiveContext(`@prefix` = ListMap("name" -> "https://example.com/name"),
                        remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext())))
        Future {
          encoder
            .fromActiveContext(defaultContext)
            .map(_.noSpaces)
            .map(_ shouldBe """["https://remote.example.org",{"name":"https://example.com/name"}]""")
            .getOrElse(fail)
        }
      }
      "exists of two remote contexts and a local context" in {
        val defaultContext =
          ActiveContext(
            `@prefix` = ListMap("name" -> "https://example.com/name"),
            remotes = List(NamedActiveContext("https://remote.example.org", ActiveContext()),
                           NamedActiveContext("https://remote2.example.org", ActiveContext()))
          )
        Future {
          encoder
            .fromActiveContext(defaultContext)
            .map(_.noSpaces)
            .map(_ shouldBe """["https://remote.example.org","https://remote2.example.org",{"name":"https://example.com/name"}]""")
            .getOrElse(fail)
        }
      }
    }
    "encode a @list(@int)" in Future {
      encoder.fromCollection(List(1, 2, 3), `@list`(`@int`))(ActiveContext()).json.noSpaces shouldBe "[1,2,3]"
    }
    "encode a @tuple(@list(int))(@list(@string))" in Future {
      encoder
        .fromTuple((List(1, 2, 3), List("a")), `@tuple`(`@list`(`@int`), `@list`(`@string`)))(ActiveContext())
        .json
        .noSpaces shouldBe "[[1,2,3],[\"a\"]]"
      encoder
        .fromTuple((List(1, 2, 3), List[String]()), `@tuple`(`@list`(`@int`), `@list`(`@string`)))(ActiveContext())
        .json
        .noSpaces shouldBe "[[1,2,3],[]]"
    }
  }
}
