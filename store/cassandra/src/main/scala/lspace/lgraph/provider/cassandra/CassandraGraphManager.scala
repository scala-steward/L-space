package lspace.lgraph.provider.cassandra

import com.outworkers.phantom.dsl._
import lspace.lgraph.{GraphManager, LGraph, LGraphIdProvider}
import monix.eval.Task
import monix.execution.atomic.AtomicLong

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Success

class CassandraGraphManager[G <: LGraph](override val graph: G, override val database: CassandraGraph)
    extends GraphManager[G](graph)
    with DatabaseProvider[CassandraGraph]
    /*with DatabaseProvider[LGraphDatabase] */ {

  Await.result(
    Future.sequence(
      Seq(
        database.states.create.ifNotExists().future()
      )
    ),
    60.seconds
  )

  private var idState = State("all", 1000L, graph.iri)
  lazy val idProvider: LGraphIdProvider = new LGraphIdProvider {
    protected def newIdRange: Vector[Long] = {
      val idStates = Await
        .result(database.states.findByName("all"), 5.seconds)
      if (idStates.size > 1) throw new Exception("???")
      idStates.headOption
        .map { idSpace =>
          if (idSpace.last >= idState.last) {
            idState = State("all", idSpace.last + 50000, graph.iri)
            Await.result(database.states.storeRecord(State("all", idState.last, graph.iri)), 10.seconds)
            idSpace.last.to(idState.last - 1).toVector
          } else {
            idState = State("all", idState.last + 50000, graph.iri)
            Await.result(database.states.storeRecord(State("all", idState.last, graph.iri)), 10.seconds)
            idSpace.last.to(idState.last - 1).toVector
          }
        }
        .getOrElse {
          idState = State("all", idState.last + 50000, graph.iri)
          Await.result(database.states.storeRecord(State("all", idState.last, graph.iri)), 10.seconds)
          (idState.last - 50000).to(idState.last - 1).toVector
        }
    }
  }

  def purge: Task[Unit] = for { _ <- Task.deferFuture(database.truncateAsync()) } yield ()

  def close(): Task[Unit] =
    for {
      id <- graph.idProvider.next
      _  <- Task.deferFuture(database.states.storeRecord(State("all", id, graph.iri)))
    } yield database.shutdown()
}
