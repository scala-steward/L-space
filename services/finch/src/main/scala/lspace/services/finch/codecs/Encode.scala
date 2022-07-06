package lspace.services.finch.codecs

import cats.effect.Effect
import com.twitter.io.Buf
import io.finch.{EncodeStream, StreamInstances}
import lspace.codec.{ActiveContext, ContextedT}
import lspace.encode.{EncodeJson, EncodeJsonLD, EncodeText}
import lspace.librarian.traversal.Collection
import lspace.services.codecs
import lspace.services.codecs.{Application, Text}
import lspace.structure.ClassType
import monix.eval.Coeval
import shapeless.=:!=

object Encode {

  implicit def toTextPlain[In](implicit
    e: EncodeText[In, Coeval],
    activeContext: ActiveContext,
    encoder: lspace.services.codecs.Decode[In, Text.Plain, String, Coeval]
  ): io.finch.Encode.Aux[In, Text.Plain] =
    io.finch.Encode
      .instance[In, Text.Plain]((a, cs) => Buf.ByteArray.Owned(e.encode(activeContext)(a).value().getBytes(cs.name)))

  implicit def toTextHtml[In](implicit
    e: EncodeText[In, Coeval],
    activeContext: ActiveContext,
    encoder: lspace.services.codecs.Decode[In, Text.Html, String, Coeval]
  ): io.finch.Encode.Aux[In, Text.Html] =
    io.finch.Encode
      .instance[In, Text.Html]((a, cs) => Buf.ByteArray.Owned(e.encode(activeContext)(a).value().getBytes(cs.name)))

  implicit def toApplicationJson[In](implicit
    e: EncodeJson[In, Coeval],
    activeContext: ActiveContext,
    encoder: lspace.services.codecs.Decode[In, Application.Json, String, Coeval]
  ): io.finch.Encode.Aux[In, Application.Json] =
    io.finch.Encode
      .instance[In, Application.Json]((a, cs) =>
        Buf.ByteArray.Owned(e.encode(activeContext)(a).value().getBytes(cs.name))
      )

  implicit def toApplicationJsonLD[In](implicit
    e: EncodeJsonLD[In, Coeval],
    activeContext: ActiveContext,
    encoder: lspace.services.codecs.Decode[In, Application.JsonLD, String, Coeval]
  ): io.finch.Encode.Aux[In, Application.JsonLD] =
    io.finch.Encode
      .instance[In, Application.JsonLD]((a, cs) =>
        Buf.ByteArray.Owned(e.encode(activeContext)(a).value().getBytes(cs.name))
      )

  object streamEncoders extends StreamInstances {
    implicit def encodeJsonLDFs2Stream[In, F[_]: Effect](implicit
      In: io.finch.Encode.Aux[In, Application.JsonLD]
    ): EncodeStream.Aux[F, _root_.fs2.Stream, In, codecs.Application.JsonLD] =
      new EncodeNewLineDelimitedEffectFs2Stream[F, In, codecs.Application.JsonLD]
  }

  implicit def encodeJsonLDFs2Stream[In, F[_]: Effect](implicit
    In: io.finch.Encode.Aux[In, Application.JsonLD]
  ): EncodeStream.Aux[F, _root_.fs2.Stream, In, codecs.Application.JsonLD] =
    streamEncoders.encodeJsonLDFs2Stream[In, F]
}
