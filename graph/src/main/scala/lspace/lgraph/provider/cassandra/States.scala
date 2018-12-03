package lspace.lgraph.provider.cassandra

import com.outworkers.phantom.dsl._

import scala.concurrent.Future

abstract class States extends Table[States, State] {
  object name extends StringColumn with PartitionKey
  object id   extends LongColumn

  def findByName(name: String): Future[Option[State]] = {
    select.where(_.name eqs name).one()
  }
}
