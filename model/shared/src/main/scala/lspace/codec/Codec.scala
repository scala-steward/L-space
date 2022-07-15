package lspace
package codec

import java.nio.charset.StandardCharsets
import scala.util.Try

import lspace.classtypes._

/** @tparam H
  *   higher (decoded) type
  * @tparam L
  *   lower (encoded) type
  */
trait Codec[H, L] extends Decoder[H, L] with Encoder[H, L]

object Codec:

  def codecTrain[H, HL, L](implicit codecHHL: Codec[H, HL], codecHLL: Codec[HL, L]): Codec[H, L] =
    new Codec[H, L] {

      override def decode(value: L): Either[Throwable, H] =
        codecHLL.decode(value).flatMap(codecHHL.decode)

      override def encode(value: H): L = codecHLL.encode(codecHHL.encode(value))
    }

  given intCodec: Conversion[Int, OrphanValue[Int]]             = v => OrphanValue(v, IntType.int)
  given doubleCodec: Conversion[Double, OrphanValue[Double]]    = v => OrphanValue(v, DoubleType.double)
  given longCodec: Conversion[Long, OrphanValue[Long]]          = v => OrphanValue(v, LongType.long)
  given stringCodec: Conversion[String, OrphanValue[String]]    = v => OrphanValue(v, StringType.string)
  given booleanCodec: Conversion[Boolean, OrphanValue[Boolean]] = v => OrphanValue(v, BooleanType)

  type NodeEdges[X] = X match
    case EmptyTuple             => EmptyTuple
    case (name, value) *: edges => OrphanEdge[name, Node, value] *: NodeEdges[edges]
    case (name, value)          => OrphanEdge[name, Node, value] *: EmptyTuple

  given ccCodec[cc <: Product](using edges: NodeEdges[cc]): Conversion[cc, Node] =
    v => OrphanNode(Set.empty, Set.empty) // Tuple.fromProductTyped(v)
