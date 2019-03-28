package lspace.lgraph

import lspace.lgraph.store.LStore
import lspace.structure.util.IdProvider
import monix.eval.Task

trait LGraphIdProvider extends IdProvider {
  protected def newIdRange: Vector[Long]
  private var id                   = newIdRange.iterator
  private[this] val idProviderLock = new Object
  def next: Task[Long] =
    Task {
      idProviderLock.synchronized {
        if (id.hasNext) id.next()
        else {
          id = newIdRange.iterator
          id.next()
        }
      }
    }.executeOn(LStore.ec)
}
