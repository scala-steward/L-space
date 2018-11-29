package lspace.lgraph.store

import lspace.lgraph.{GraphManager, LGraph}

trait StoreProvider {
  def iri: String

  def stateManager[G <: LGraph](graph: G): GraphManager[G]
  def dataManager[G <: LGraph](graph: G): StoreManager[G]
  def nsManager[G <: LGraph](graph: G): StoreManager[G]
  def nsIndexManager[G <: LGraph](graph: G): StoreManager[G]
  def indexManager[G <: LGraph](graph: G): StoreManager[G]
  def indexIndexManager[G <: LGraph](graph: G): StoreManager[G]
}
