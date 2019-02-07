package lspace.services.codecs

import com.twitter.io.Buf
import io.finch.Text
import lspace.encode.{EncodeJson, EncodeJsonLD, EncodeText}

object Encode {
  type JsonLD[A] = io.finch.Encode.Aux[A, Application.JsonLD]
  type Json[A]   = io.finch.Encode.Aux[A, io.finch.Application.Json]
  type Text[A]   = io.finch.Encode.Aux[A, Text.Plain]

  implicit def encodeArgonautText[A](implicit e: EncodeText[A]): Text[A] = {
    io.finch.Encode.instance[A, Text.Plain]((a, cs) => Buf.ByteArray.Owned(e.encode(a).getBytes(cs.name)))
  }

  implicit def encodeArgonautJson[A](implicit e: EncodeJson[A]): Json[A] = {
    io.finch.Encode
      .instance[A, io.finch.Application.Json]((a, cs) => Buf.ByteArray.Owned(e.encode(a).getBytes(cs.name)))
  }

  implicit def encodeArgonautJsonLD[A](implicit e: EncodeJsonLD[A]): JsonLD[A] = {
    io.finch.Encode.instance[A, Application.JsonLD]((a, cs) => Buf.ByteArray.Owned(e.encode(a).getBytes(cs.name)))
  }
}
