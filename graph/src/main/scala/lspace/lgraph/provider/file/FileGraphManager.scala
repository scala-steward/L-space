package lspace.lgraph.provider.file

import lspace.lgraph.{GraphManager, LGraph, LGraphIdProvider}
import monix.execution.atomic.Atomic

object FileGraphManager {
  def apply[G <: LGraph](graph: G): FileGraphManager[G] = new FileGraphManager(graph)
}

class FileGraphManager[G <: LGraph](override val graph: G) extends GraphManager[G](graph) {

  override def idProvider: LGraphIdProvider = new LGraphIdProvider {
    protected def newIdRange: Vector[Long] = Vector()
    private val id                         = Atomic(1000l)
    override def next: Long                = id.incrementAndGet()
  }

  override def close(): Unit = {}
}
