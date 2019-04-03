package lspace.lgraph.index

import lspace.lgraph.LGraph
import monix.eval.Task

abstract class IndexManager[G <: LGraph](val graph: G) {

  def close(): Task[Unit]
}
