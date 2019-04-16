package lspace.services.codecs

import io.finch.Text
import lspace.decode.{DecodeJson, DecodeJsonLD}
import monix.eval.Task

object Decode {
  type JsonLD[A] = io.finch.Decode.Aux[A, Application.JsonLD]
  type Json[A]   = io.finch.Decode.Aux[A, io.finch.Application.Json]
  type Text[A]   = io.finch.Decode.Aux[A, Text.Plain]

  import io.finch.internal.HttpContent
  implicit def decodeArgonautText[A](implicit e: DecodeJsonLD[A]): Text[Task[A]] =
    io.finch.Decode.instance[Task[A], Text.Plain] { (b, cs) =>
      Right(e.decode(b.asString(cs)))
    }

  implicit def decodeJson[A](implicit e: DecodeJson[A]): Json[Task[A]] =
    io.finch.Decode.instance[Task[A], io.finch.Application.Json] { (b, cs) =>
      Right(e.decode(b.asString(cs)))
    }

  implicit def decodeJsonLD[A](implicit e: DecodeJsonLD[A]): JsonLD[Task[A]] =
    io.finch.Decode.instance[Task[A], Application.JsonLD] { (b, cs) =>
      Right(e.decode(b.asString(cs)))
    }

}
