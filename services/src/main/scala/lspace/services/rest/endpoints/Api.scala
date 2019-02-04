package lspace.services.rest.endpoints

import cats.effect.IO
import com.twitter.finagle.http.Request
import io.finch.Endpoint

trait Api extends Endpoint.Module[IO] {
  def api: Endpoint[IO, _]
}
