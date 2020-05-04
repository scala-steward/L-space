package lspace.js

import lspace._

import scala.scalajs.js.annotation._

@JSExportTopLevel("LSpace")
object LSpace {
  @JSExport
  def graph(iri: String): JGraph = lspace.Graph.apply(iri)

  @JSExportAll
  type JGraph = Graph
}
