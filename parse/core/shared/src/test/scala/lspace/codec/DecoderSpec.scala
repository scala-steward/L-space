package lspace.codec

import lspace.datatype.DataType
import lspace.structure.{Ontology, Property}
import org.scalatest.{AsyncWordSpec, Matchers}
import scribe.Level
import scribe.format._
import scala.concurrent.duration._

trait DecoderSpec extends AsyncWordSpec with Matchers {
  def decoder: lspace.codec.Decoder

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Trace), formatter = Formatter.enhanced)
    .replace()

  "The Decoder" should {
    "parse any ontology from schema.org" in {
      decoder
        .toOntology("https://schema.org/Person")(ActiveContext())
        .map { ontology =>
//          Ontology.ontologies.all.foreach(o => println(s"${o.iri} ${o.label()}")) // ${o.comment()}"))
//          Property.properties.all.foreach(p => println(s"${p.iri} ${p.label()}")) // ${p.comment()}"))
          ontology.iri shouldBe "https://schema.org/Person"
//          println(ontology.properties().map(_.iri))
          ontology
            .properties("https://schema.org/additionalName")
            .exists(_.range(lspace.Label.D.`@string`.iri).isDefined) shouldBe true
          ontology.properties("https://schema.org/colleagues").isDefined shouldBe true
        }
        .timeout(15.seconds)
        .runToFuture
    }
  }
}
