package lspace.services.rest.endpoints.util

import cats.Applicative
import io.finch.{Endpoint, EndpointResult, Input, Output, Trace}
import shapeless.HNil

class MatchParam[F[_]](name: String)(implicit F: Applicative[F]) extends Endpoint[F, HNil] {
  final def apply(input: Input): EndpointResult[F, HNil] =
    if (input.request.containsParam(name))
      EndpointResult.Matched(
        input,
        Trace.empty,
        F.pure(Output.HNil)
      )
    else EndpointResult.NotMatched[F]
}

object MatchParam {
  def apply[F[_]](name: String)(implicit F: Applicative[F]): MatchParam[F] =
    new MatchParam[F](name)
}
