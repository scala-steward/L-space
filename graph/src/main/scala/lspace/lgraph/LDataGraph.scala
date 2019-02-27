package lspace.lgraph

import lspace.lgraph.index.LIndex
import lspace.structure._
import lspace.structure.index.Index
import monix.eval.Task
import monix.execution.CancelableFuture

import scala.concurrent.Future

trait LDataGraph extends LGraph with DataGraph {

  def index: LIndexGraph

  protected def `@idIndex`: Index =
    LIndex(__[Any, Any].has(Property.default.`@id`).has(Property.default.`@ids`).untyped)
  protected def `@typeIndex`: Index = LIndex(__[Any, Any].has(Property.default.`@type`).untyped)

  override def persist: CancelableFuture[Unit] = {
    Task
      .gatherUnordered(
        Seq(
          storeManager.persist,
          ns.storeManager.persist,
          index.storeManager.persist
        ))
      .foreachL(f => Task.unit)
      .runToFuture(monix.execution.Scheduler.global)
  }
}
