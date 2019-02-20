package lspace.lgraph

import lspace.structure.util.IdProvider

trait LGraphIdProvider extends IdProvider {
  protected def newIdRange: Vector[Long]
  private var id                   = newIdRange.iterator
  private[this] val idProviderLock = new Object
  def next: Long = idProviderLock.synchronized {
    if (id.hasNext) id.next()
    else {
      id = newIdRange.iterator
      id.next()
    }
  }
}
