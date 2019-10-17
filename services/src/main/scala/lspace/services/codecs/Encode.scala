package lspace.services.codecs

import cats.effect.Effect
import com.twitter.io.Buf
import io.finch.{EncodeStream, StreamInstances, Text}
import lspace.codec.{ActiveContext, ContextedT}
import lspace.encode.{EncodeJson, EncodeJsonLD, EncodeText}
import lspace.librarian.traversal.Collection
import lspace.services.codecs
import lspace.structure.ClassType
import shapeless.=:!=

object Encode {
  type GraphQL[A]  = io.finch.Encode.Aux[A, Application.GraphQL]
  type JsonLD[A]   = io.finch.Encode.Aux[A, Application.JsonLD]
  type Json[A]     = io.finch.Encode.Aux[A, io.finch.Application.Json]
  type Text[A]     = io.finch.Encode.Aux[A, Text.Plain]
  type TextHTML[A] = io.finch.Encode.Aux[A, Text.Html]

  implicit def encodeText[A](implicit e: EncodeText[A], activeContext: ActiveContext): Text[A] = {
    io.finch.Encode
      .instance[A, Text.Plain]((a, cs) => Buf.ByteArray.Owned(e.encode(activeContext)(a).getBytes(cs.name)))
  }
  implicit def encodeText2[A](implicit e: EncodeText[A], activeContext: ActiveContext): TextHTML[A] = {
    io.finch.Encode
      .instance[A, Text.Html]((a, cs) => Buf.ByteArray.Owned(e.encode(activeContext)(a).getBytes(cs.name)))
  }

  implicit def encodeJson[A](implicit e: EncodeJson[A], activeContext: ActiveContext): Json[A] = {
    io.finch.Encode
      .instance[A, io.finch.Application.Json]((a, cs) =>
        Buf.ByteArray.Owned(e.encode(activeContext)(a).getBytes(cs.name)))
  }

  implicit def encodeJsonLD[A](implicit e: EncodeJsonLD[A], activeContext: ActiveContext): JsonLD[A] = {
    io.finch.Encode.instance[A, Application.JsonLD]((a, cs) =>
      Buf.ByteArray.Owned(e.encode(activeContext)(a).getBytes(cs.name)))
  }

  object streamEncoders extends StreamInstances {
    implicit def encodeJsonLDFs2Stream[A, F[_]: Effect](
        implicit
        A: JsonLD[A]): EncodeStream.Aux[F, _root_.fs2.Stream, A, codecs.Application.JsonLD] =
      new EncodeNewLineDelimitedEffectFs2Stream[F, A, codecs.Application.JsonLD]
  }

  implicit def encodeJsonLDFs2Stream[A, F[_]: Effect](
      implicit
      A: JsonLD[A]): EncodeStream.Aux[F, _root_.fs2.Stream, A, codecs.Application.JsonLD] =
    streamEncoders.encodeJsonLDFs2Stream[A, F]

  implicit def encodeTextException: io.finch.Encode.Aux[Exception, Text.Plain] = {
    io.finch.Encode.encodeExceptionAsTextPlain
  }
}
