package lspace.lgraph.provider.mem

import lspace.lgraph.LGraph
import lspace.lgraph.index.{IndexManager, IndexProvider}

class MemIndexProvider extends IndexProvider {
  def nsIndexManager[G <: LGraph](graph: G): IndexManager[G]    = new MemIndexManager(graph)
  def indexManager[G <: LGraph](graph: G): IndexManager[G]      = new MemIndexManager(graph)
  def indexIndexManager[G <: LGraph](graph: G): IndexManager[G] = new MemIndexManager(graph)
}
