package lspace.lgraph

import lspace.librarian.structure.NameSpaceGraph
import monix.eval.Task
import monix.execution.CancelableFuture

trait LNSGraph extends LDataGraph with NameSpaceGraph {
  def index: LIndexGraph
  def graph: LGraph

  override def persist: CancelableFuture[Unit] = {
    Task
      .gatherUnordered(
        Seq(
          Task.fromFuture(storeManager.persist),
          Task.fromFuture(index.persist)
        ))
      .foreachL(f => Task.unit)
      .runToFuture(monix.execution.Scheduler.global)
  }
}
