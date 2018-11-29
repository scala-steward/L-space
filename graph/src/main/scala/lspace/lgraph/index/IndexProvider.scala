package lspace.lgraph.index

import lspace.lgraph.LGraph

trait IndexProvider {
  def nsIndexManager[G <: LGraph](graph: G): IndexManager[G]
  def indexManager[G <: LGraph](graph: G): IndexManager[G]
  def indexIndexManager[G <: LGraph](graph: G): IndexManager[G]
}
