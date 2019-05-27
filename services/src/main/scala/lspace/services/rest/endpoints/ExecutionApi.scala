package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace._
import lspace.codec.ContextedT
import lspace.librarian.traversal.Collection

trait ExecutionApi extends Api {

  def query: Endpoint[IO, _root_.fs2.Stream[IO, ContextedT[Collection[Any, ClassType[Any]]]]]
  def mutate: Endpoint[IO, Unit]
  def ask: Endpoint[IO, Boolean]
  def subscribe: Endpoint[IO, List[Node]]
}
