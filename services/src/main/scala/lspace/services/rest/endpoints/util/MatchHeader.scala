package lspace.services.rest.endpoints.util

import cats.Applicative
import cats.effect.IO
import io.finch._
import shapeless.HNil

class MatchHeader[F[_]](key: String, value: String)(implicit
                                                    F: Applicative[F])
    extends Endpoint[F, HNil] {
  final def apply(input: Input): EndpointResult[F, HNil] = {
    input.request.headerMap.get(key) match {
      case Some(v) if v == value =>
        EndpointResult.Matched(
          input,
          Trace.empty,
          F.pure(Output.HNil)
        )
      case _ => EndpointResult.NotMatched[F]
    }
  }

  final override def toString: String = s"$key: $value"
}

object MatchHeader {
  val beGraphQL = new MatchHeader[IO]("Content-Type", "application/graphql")
}
