package lspace.lgraph

import lspace.lgraph.index.{IndexManager, IndexProvider}
import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.structure.History
import lspace.structure.util.IdProvider

object LHistory {
  def apply(iri: String, storeProvider: StoreProvider, indexProvider: IndexProvider): LGraph = {
    val _iri           = iri

    val graph = new LHistory {
      val iri: String  = _iri
      private val self = this

      lazy val storeManager: StoreManager[this.type] = storeProvider.dataManager(this)

      lazy val ns: LNSGraph = new LNSGraph {
        def iri: String = _iri + ".ns"

        lazy val graph: LGraph = self

        private val _thisgraph = thisgraph
        lazy val index: LIndexGraph = new LIndexGraph {
          def iri: String = _iri + ".ns" + ".index"

          lazy val graph: LGraph      = _thisgraph

          lazy val storeManager: StoreManager[this.type] = storeProvider.nsIndexManager(this)
          lazy val indexManager: IndexManager[this.type] = indexProvider.nsManager(this)
        }
        lazy val storeManager: StoreManager[this.type] = storeProvider.nsManager(this)
      }

      lazy val index: LIndexGraph = new LIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: LGraph = self

        lazy val storeManager: StoreManager[this.type] = storeProvider.indexManager(this)
        lazy val indexManager: IndexManager[this.type] = indexProvider.dataManager(this)
      }

      val stateManager: GraphManager[this.type]             = storeProvider.stateManager(this)
      override protected[lspace] def idProvider: IdProvider = stateManager.idProvider
    }
    graph.init
    graph
  }
}

trait LHistory extends LDataGraph with History {}
