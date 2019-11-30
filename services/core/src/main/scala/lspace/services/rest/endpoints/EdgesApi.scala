package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace.structure.Edge

trait EdgesApi {
  def apply(): Endpoint[IO, List[Edge[Any, Any]]]
  def hasId(id: Long): Endpoint[IO, Edge[Any, Any]]
}
