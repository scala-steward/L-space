package lspace.util

import lspace.structure.Graph

case class MemGraphConfig(name: String) extends GraphConfig {
  def toGraph: Graph = Graph(name)
}
