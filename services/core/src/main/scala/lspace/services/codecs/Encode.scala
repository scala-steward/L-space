package lspace.services.codecs

import lspace.codec.ActiveContext
import lspace.encode._

class Encode[In, Codec, Out, F[_]](encoder: lspace.encode.Encode.Aux[In, F, Out]) {
  def encode(in: In, activeContext: ActiveContext): F[Out] = encoder.encode(activeContext)(in)
}

object Encode {

  implicit def textPlain[In, Codec, Out, F[_]](implicit
    encoder: EncodeText.Aux[In, F, Out]
  ): Encode[In, Text.Plain, Out, F] =
    new Encode[In, Text.Plain, Out, F](encoder)

  implicit def textHtml[In, Codec, Out, F[_]](implicit
    encoder: EncodeText.Aux[In, F, Out]
  ): Encode[In, Text.Html, Out, F] =
    new Encode[In, Text.Html, Out, F](encoder)

  implicit def applicationJson[In, Codec, Out, F[_]](implicit
    encoder: EncodeText.Aux[In, F, Out]
  ): Encode[In, Application.Json, Out, F] =
    new Encode[In, Application.Json, Out, F](encoder)

  implicit def applicationJsonLD[In, Codec, Out, F[_]](implicit
    encoder: EncodeText.Aux[In, F, Out]
  ): Encode[In, Application.JsonLD, Out, F] =
    new Encode[In, Application.JsonLD, Out, F](encoder)
}
