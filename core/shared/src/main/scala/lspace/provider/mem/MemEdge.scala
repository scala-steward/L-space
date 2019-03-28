package lspace.provider.mem

import lspace.structure._
import monix.eval.Task

object MemEdge {}

trait MemEdge[S, E] extends MemResource[Edge[S, E]] with Edge[S, E] {

  override def remove(): Task[Unit] = {
    for {
      _ <- super.remove()
      _ <- inV.removeIn(this)
      _ <- outV.removeOut(this)
    } yield ()
  }
}
