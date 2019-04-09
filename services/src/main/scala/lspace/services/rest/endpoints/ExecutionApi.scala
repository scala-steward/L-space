package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace.structure.Node

trait ExecutionApi {
  def query: Endpoint[IO, List[Node]]
  def mutate: Endpoint[IO, Unit]
  def ask: Endpoint[IO, Boolean]
  def subscribe: Endpoint[IO, List[Node]]
}
