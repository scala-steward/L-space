package lspace.js

import lspace._
import scala.scalajs.js.annotation._

@JSExportTopLevel("LSpace")
object LSpace {
  @JSExport
  def graph(iri: String): Graph = lspace.Graph.apply(iri)

  @JSExport("Graph")
  object Graph {
    def apply(iri: String): Graph = lspace.Graph.apply(iri)
  }
}
