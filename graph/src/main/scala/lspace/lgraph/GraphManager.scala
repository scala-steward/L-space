package lspace.lgraph

import monix.eval.Task

abstract class GraphManager[G <: LGraph](val graph: G) {

  def idProvider: LGraphIdProvider

  def purge: Task[Unit]

  def close(): Task[Unit]
}
