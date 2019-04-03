package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace.structure.Node

trait NodesApi {
  def apply(): Endpoint[IO, List[Node]]
  def hasId(id: Long): Endpoint[IO, Node]
}
