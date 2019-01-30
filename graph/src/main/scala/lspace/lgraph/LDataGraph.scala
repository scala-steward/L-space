package lspace.lgraph

import lspace.lgraph.index.LIndex
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index
import monix.eval.Task
import monix.execution.CancelableFuture

import scala.concurrent.Future

trait LDataGraph extends LGraph with DataGraph {
  protected def `@idIndex`: Index =
    LIndex(__[Any, Any].has(Property.default.`@id`).has(Property.default.`@ids`).untyped)
  protected def `@typeIndex`: Index = LIndex(__[Any, Any].has(Property.default.`@type`).untyped)

  override def persist: CancelableFuture[Unit] = {
    Task
      .gatherUnordered(
        Seq(
          Task.fromFuture(storeManager.persist),
          Task.fromFuture(ns.persist),
          Task.fromFuture(index.persist)
        ))
      .foreachL(f => Task.unit)
      .runToFuture(monix.execution.Scheduler.global)
  }
}
