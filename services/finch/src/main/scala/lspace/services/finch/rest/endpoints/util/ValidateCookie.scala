package lspace.services.rest.endpoints.util

import cats.Applicative
import io.finch._
import shapeless.HNil

class ValidateCookie[F[_]](key: String, f: String => Boolean)(implicit F: Applicative[F]) extends Endpoint[F, HNil] {
  final def apply(input: Input): EndpointResult[F, HNil] =
    input.request.cookies
      .get(key)
      .filter(_.httpOnly)
      .map { cookie =>
        if (f(cookie.value))
          EndpointResult.Matched(
            input,
            Trace.empty,
            F.pure(Output.HNil)
          )
        else EndpointResult.NotMatched[F]
      }
      .getOrElse(EndpointResult.NotMatched[F])
}

object ValidateCookie {
  def apply[F[_]](key: String, f: String => Boolean)(implicit F: Applicative[F]): ValidateCookie[F] =
    new ValidateCookie[F](key, f)
}
