package lspace.lgraph.provider.file

import lspace.lgraph.{GraphManager, LGraph, LGraphIdProvider}
import monix.execution.CancelableFuture
import monix.execution.atomic.Atomic

object FileGraphManager {
  def apply[G <: LGraph](graph: G, path: String): FileGraphManager[G] = new FileGraphManager(graph, path)
}

class FileGraphManager[G <: LGraph](override val graph: G, path: String) extends GraphManager[G](graph) {

  override def idProvider: LGraphIdProvider = new LGraphIdProvider {
    protected def newIdRange: Vector[Long] = Vector()
    private val id                         = Atomic(1000l)
    override def next: Long                = id.incrementAndGet()
  }

  override def close(): CancelableFuture[Unit] = CancelableFuture.unit
}
