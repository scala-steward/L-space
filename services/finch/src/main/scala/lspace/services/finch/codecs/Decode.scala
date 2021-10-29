package lspace.services.finch.codecs

import com.twitter.io.Buf
import lspace.codec.ActiveContext
import lspace.services.codecs.{Application, Text}
import lspace.decode._
import monix.eval.Task

object Decode {
  type GraphQL[A] = io.finch.Decode.Aux[A, Application.GraphQL]
  type JsonLD[A]  = io.finch.Decode.Aux[A, Application.JsonLD]
  type Json[A]    = io.finch.Decode.Aux[A, io.finch.Application.Json]
  type Text[A]    = io.finch.Decode.Aux[A, Text.Plain]

  import io.finch.internal.HttpContent

  implicit def fromTextPlain[Out](implicit
    activeContext: ActiveContext,
    decoder: lspace.services.codecs.Decode[String, Text.Plain, Out, Task]
  ): io.finch.Decode.Aux[Task[Out], Text.Plain] =
    io.finch.Decode.instance[Task[Out], Text.Plain] { (b, cs) =>
      Right(decoder.decode(b.asString(cs), activeContext))
    }

  implicit def fromApplicationJson[Out](implicit
    activeContext: ActiveContext,
    decoder: lspace.services.codecs.Decode[String, Application.Json, Out, Task]
  ): io.finch.Decode.Aux[Task[Out], Application.Json] =
    io.finch.Decode.instance[Task[Out], Application.Json] { (b, cs) =>
      Right(decoder.decode(b.asString(cs), activeContext))
    }

  implicit def fromApplicationJsonLD[Out](implicit
    activeContext: ActiveContext,
    decoder: lspace.services.codecs.Decode[String, Application.JsonLD, Out, Task]
  ): io.finch.Decode.Aux[Task[Out], Application.JsonLD] =
    io.finch.Decode.instance[Task[Out], Application.JsonLD] { (b, cs) =>
      Right(decoder.decode(b.asString(cs), activeContext))
    }

  implicit def fromApplicationGraphQL[Out](implicit
    activeContext: ActiveContext,
    decoder: lspace.services.codecs.Decode[String, Application.GraphQL, Out, Task]
  ): io.finch.Decode.Aux[Task[Out], Application.GraphQL] =
    io.finch.Decode.instance[Task[Out], Application.GraphQL] { (b, cs) =>
      Right(decoder.decode(b.asString(cs), activeContext))
    }
}
