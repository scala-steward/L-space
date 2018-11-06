package lspace.services.rest.endpoints

import cats.effect.IO
import io.finch.Endpoint

trait Service extends Endpoint.Module[IO] {
//  def api: Endpoint[IO, _]
}
