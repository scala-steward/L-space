package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace._
import lspace.librarian.traversal.Collection

trait ExecutionApi extends Api {

  def query: Endpoint[IO, Collection[Any, ClassType[Any]]]
  def mutate: Endpoint[IO, Unit]
  def ask: Endpoint[IO, Boolean]
  def subscribe: Endpoint[IO, List[Node]]
}
