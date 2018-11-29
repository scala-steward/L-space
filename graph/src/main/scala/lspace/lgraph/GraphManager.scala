package lspace.lgraph

abstract class GraphManager[G <: LGraph](val graph: G) {

  def idProvider: LGraphIdProvider

  def close(): Unit
}
