package lspace.encode

import lspace.codec.{ActiveContext, NativeTypeEncoder}
import lspace.structure.Node

trait EncodeJsonLD[A] extends Encode[A] {
  def encode: A => String
}

object EncodeJsonLD {

  import lspace.codec.JsonInProgress._

  implicit def nodeToJsonLD[T <: Node, Json](implicit encoder: lspace.codec.Encoder) = new EncodeJsonLD[T] {
    def encode = (node: T) => encoder(node)
  }

  implicit def nodesToJsonLD[T <: Node](implicit encoder: lspace.codec.Encoder) = {
    implicit val bd: NativeTypeEncoder.Aux[encoder.Json] =
      encoder.baseEncoder.asInstanceOf[NativeTypeEncoder.Aux[encoder.Json]]

    new EncodeJsonLD[List[T]] {
      def encode: List[T] => String =
        (nodes: List[T]) => encoder.fromAny(nodes)(ActiveContext()).withContext.toString
    }
  }

  implicit val encodeJsonLDJson = new EncodeJsonLD[String] {
    val encode = (json: String) => json
  }
}
