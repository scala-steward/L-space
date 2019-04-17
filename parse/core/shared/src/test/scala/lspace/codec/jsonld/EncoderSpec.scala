package lspace.codec.jsonld

import lspace._
import lspace.Label.D._
import lspace.Label.P._
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.provider.mem.MemGraph
import lspace.structure.SampledGraph
import org.scalatest.{AsyncWordSpec, FutureOutcome, Matchers}
import scribe.Level
import scribe.format.Formatter

import scala.collection.immutable.ListMap

abstract class EncoderSpec(encoder: Encoder) extends AsyncWordSpec with Matchers {

  import encoder._
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

  "The Encoder" should {
    "use a default context if provided" in {
      val defaultContext = ActiveContext(
        `@prefix` = ListMap("naam" -> "name"),
        definitions = Map("name"   -> ActiveProperty(`@type` = `@string` :: Nil, property = Property("name")))
      )
      (for {
        sample <- initTask
        stan = sample.persons.Stan.person
        joip = encoder.fromNode(stan)(defaultContext)
        json = joip.json
        ac   = joip.activeContext
        _ = encoder.fromActiveContext(ac).map(_.noSpaces) shouldBe Some(
          """{"naam":{"@id":"name","@type":"@string"},"1":"https://example.org/"}""")
        _ = println(joip.withContext.noSpaces)
      } yield succeed).runToFuture
    }
  }
}
