package lspace.lgraph.provider.elasticsearch

import lspace.lgraph.LGraph
import lspace.lgraph.index.{IndexManager, IndexProvider}

class ESIndexProvider extends IndexProvider {
  def nsIndexManager[G <: LGraph](graph: G): IndexManager[G]    = new ESIndexManager(graph)
  def indexManager[G <: LGraph](graph: G): IndexManager[G]      = new ESIndexManager(graph)
  def indexIndexManager[G <: LGraph](graph: G): IndexManager[G] = new ESIndexManager(graph)
}
