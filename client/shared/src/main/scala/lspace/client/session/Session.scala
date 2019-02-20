package lspace.client.session

import lspace.structure.{IriResource, Property}
import lspace.structure.OntologyDef

object Session extends OntologyDef(lspace.NS.vocab.Lspace + "Session", Set(), "Session", "A session ..") {
  object keys
  override lazy val properties: List[Property] = Nil
  trait Properties
}
trait Session extends IriResource
