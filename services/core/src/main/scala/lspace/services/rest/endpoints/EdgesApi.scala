package lspace.services.rest.endpoints

import lspace.structure.Edge
import sttp.tapir._

trait EdgesApi {
  def apply(): Endpoint[IO, List[Edge[Any, Any]]]
  def hasId(id: Long): Endpoint[IO, Edge[Any, Any]]
}
