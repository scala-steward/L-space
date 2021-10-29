package lspace.services.codecs

import lspace.codec.ActiveContext
import lspace.decode._

class Decode[In, Codec, Out, F[_]](decoder: lspace.decode.Decode.Aux[Out, F, In]) {
  def decode(in: In, activeContext: ActiveContext): F[Out] = decoder.decode(activeContext)(in)
}

object Decode {

  implicit def textPlain[In, Codec, Out, F[_]](implicit
    decoder: DecodeText.Aux[Out, F, In]
  ): Decode[In, Text.Plain, Out, F] =
    new Decode[In, Text.Plain, Out, F](decoder)

  implicit def applicationJson[In, Codec, Out, F[_]](implicit
    decoder: DecodeJson.Aux[Out, F, In]
  ): Decode[In, Application.Json, Out, F] =
    new Decode[In, Application.Json, Out, F](decoder)

  implicit def applicationJsonLD[In, Codec, Out, F[_]](implicit
    decoder: DecodeJsonLD.Aux[Out, F, In]
  ): Decode[In, Application.JsonLD, Out, F] =
    new Decode[In, Application.JsonLD, Out, F](decoder)

  implicit def applicationGraphQL[In, Codec, Out, F[_]](implicit
    decoder: DecodeGraphQL.Aux[Out, F, In]
  ): Decode[In, Application.GraphQL, Out, F] =
    new Decode[In, Application.GraphQL, Out, F](decoder)

}
