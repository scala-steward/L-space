package lspace.lgraph.provider.mem

import lspace.lgraph.LGraph
import lspace.lgraph.index.IndexManager
import monix.eval.Task

class MemIndexManager[G <: LGraph](graph: G) extends IndexManager(graph) {

  def close(): Task[Unit] = Task.unit
}
