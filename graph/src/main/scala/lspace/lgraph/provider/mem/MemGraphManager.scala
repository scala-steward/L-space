package lspace.lgraph.provider.mem

import lspace.lgraph.{GraphManager, LGraph, LGraphIdProvider}
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.atomic.Atomic

object MemGraphManager {
  def apply[G <: LGraph](graph: G): MemGraphManager[G] = new MemGraphManager(graph)
}
class MemGraphManager[G <: LGraph](override val graph: G) extends GraphManager[G](graph) {

  override def idProvider: LGraphIdProvider = new LGraphIdProvider {
    protected def newIdRange: Vector[Long] = Vector()
    private val id                         = Atomic(1000l)
    override def next: Task[Long]          = Task.now(id.incrementAndGet())
  }

  def purge: Task[Unit] = Task.unit

  override def close(): Task[Unit] = Task.unit
}
