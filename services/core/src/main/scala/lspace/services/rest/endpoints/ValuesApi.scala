package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace.structure.Value

trait ValuesApi {
  def apply(): Endpoint[IO, List[Value[Any]]]
  def hasId(id: Long): Endpoint[IO, Value[Any]]
}
