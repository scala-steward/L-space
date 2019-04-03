package lspace.lgraph.index

import lspace.lgraph.LGraph

trait IndexProvider {
  def dataManager[G <: LGraph](graph: G): IndexManager[G]
  def nsManager[G <: LGraph](graph: G): IndexManager[G]
//  def indexIndexManager[G <: LGraph](graph: G): IndexManager[G]
}
