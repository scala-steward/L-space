package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint
import lspace.structure.Resource

trait ResourcesApi {
  def apply(): Endpoint[IO, List[Resource[Any]]]
  def hasId(id: Long): Endpoint[IO, Resource[Any]]
}
