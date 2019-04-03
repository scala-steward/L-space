package lspace.lgraph

import lspace.lgraph.index.{IndexManager, LIndexes}
import lspace.provider.mem.MemGraph
import lspace.structure.index.Indexes
import lspace.structure.IndexGraph
import monix.eval.Task

trait LIndexGraph extends LGraph with IndexGraph {
  def graph: LGraph
  protected[lspace] def indexManager: IndexManager[this.type]
  override def ns: LNSGraph = graph.ns

  lazy val indexCache                    = MemGraph(s"${graph.iri}-cache")
  protected[lspace] def indexes: Indexes = new LIndexes(this) {}

//  protected val `@patternIndex`: Index = createIndex(Vector(Set()))

//  override def _storeEdge[S, E](edge: _Edge[S, E]): Unit = super._storeEdge(edge)

//  override def persist: CancelableFuture[Unit] = CancelableFuture.unit //storeManager.persist

  override def close(): Task[Unit] =
    for {
      _ <- super.close()
      _ <- indexManager.close()
    } yield ()
}
