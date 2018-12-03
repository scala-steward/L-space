package lspace.lgraph

import lspace.librarian.structure.util.IdProvider

trait LGraphIdProvider extends IdProvider {
  protected def newIdRange: Vector[Long]
  private var id = newIdRange.iterator
  def next: Long = synchronized {
    if (id.hasNext) id.next()
    else {
      id = newIdRange.iterator
      id.next()
    }
  }
}
