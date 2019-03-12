package lspace.codec

import lspace.datatype.DataType
import lspace.structure.{Ontology, Property}
import org.scalatest.{AsyncWordSpec, Matchers}
import scribe.Level
import scribe.format.Formatter

trait DecoderSpec extends AsyncWordSpec with Matchers {
  def decoder: lspace.codec.Decoder

  import monix.execution.Scheduler.Implicits.global
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
          ontology
            .properties("https://schema.org/additionalName")
            .exists(_.range(lspace.Label.D.`@string`.iri).isDefined) shouldBe true
          ontology.properties("https://schema.org/colleagues").isDefined shouldBe true
        }
        .runToFuture
    }
  }
}
