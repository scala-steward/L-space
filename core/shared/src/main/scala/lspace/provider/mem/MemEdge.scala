package lspace.provider.mem

import lspace.structure._
import monix.eval.Task

object MemEdge {}

trait MemEdge[S, E] extends MemResource[Edge[S, E]] with Edge[S, E] {

  override def remove(): Task[Unit] = {
    for {
      _ <- super.remove()
      _ <- to.removeIn(this)
      _ <- from.removeOut(this)
    } yield ()
  }
}
