package lspace.librarian.provider.mem

import lspace.librarian.structure._

object MemEdge {}

trait MemEdge[S, E] extends MemResource[Edge[S, E]] with Edge[S, E] {

  override def remove(): Unit = {
    super.remove()
    inV.removeIn(this)
    outV.removeOut(this)
  }
}
