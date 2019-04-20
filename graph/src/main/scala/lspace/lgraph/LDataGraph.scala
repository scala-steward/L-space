package lspace.lgraph

import lspace.structure._
import monix.eval.Task

trait LDataGraph extends LGraph with DataGraph {

  def index: LIndexGraph

  override def persist: Task[Unit] = {
    for {
      _ <- Task
        .gatherUnordered(
          Seq(
            storeManager.persist,
            ns.storeManager.persist,
            index.storeManager.persist
          ))
    } yield ()
  }
}
