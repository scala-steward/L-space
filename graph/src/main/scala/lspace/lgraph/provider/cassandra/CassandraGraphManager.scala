package lspace.lgraph.provider.cassandra

import com.outworkers.phantom.dsl._
import lspace.lgraph.{GraphManager, LGraph, LGraphIdProvider}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class CassandraGraphManager[G <: LGraph](override val graph: G, override val database: CassandraGraph)
    extends GraphManager[G](graph)
    with DatabaseProvider[CassandraGraph]
    /*with DatabaseProvider[LGraphDatabase] */ {

  Await.result(Future.sequence(
                 Seq(
                   database.states.create.ifNotExists().future()
                 )),
               60 seconds)

  lazy val idProvider: LGraphIdProvider = new LGraphIdProvider {
    protected def newIdRange: Vector[Long] = {
      Await
        .result(database.states.findByName("all"), 5 seconds)
        .map { idSpace =>
          val range: Vector[Long] = (idSpace.last to (idSpace.last + 999) toVector)
          database.states.storeRecord(State("all", idSpace.last + 1000))
          range
        }
        .getOrElse {
          database.states.storeRecord(State("all", 1000))
          1000l to 1999 toVector
        }
    }
  }

  def close(): Unit = {
    Await
      .result(database.states.storeRecord(State("all", graph.idProvider.next)), 5 seconds)
    database.shutdown()
  }
}
