package lspace.services.rest.endpoints.util

import cats.Applicative
import cats.effect.IO
import io.finch._
import shapeless.HNil

class MatchHeaderContains[F[_]](key: String, value: String)(implicit F: Applicative[F]) extends Endpoint[F, HNil] {
  final def apply(input: Input): EndpointResult[F, HNil] =
    input.request.headerMap.get(key) match {
      case Some(v) if v.contains(value) =>
        EndpointResult.Matched(
          input,
          Trace.empty,
          F.pure(Output.HNil)
        )
      case _ => EndpointResult.NotMatched[F]
    }

  final override def toString: String = s"$key: $value"
}

object MatchHeaderContains {
  def apply[F[_]](key: String, value: String)(implicit F: Applicative[F]): MatchHeaderContains[F] =
    new MatchHeaderContains[F](key, value)

  val beGraphQL = new MatchHeaderContains[IO]("Content-Type", "application/graphql")
}
