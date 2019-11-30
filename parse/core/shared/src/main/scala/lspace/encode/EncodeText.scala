package lspace.encode

import lspace.NS.types
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.codec.{ActiveContext, ContextedT}
import monix.eval.Coeval

trait EncodeText[In, F[_]] extends Encode[In, F] {
  type Out = String
}
object EncodeText {
  type Aux[In, F[_], Out0] = EncodeText[In, F] { type Out = Out0 }

  implicit def contextedTToJsonLD[T](implicit en: EncodeText[T, Coeval]) = new EncodeText[ContextedT[T], Coeval] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  implicit def activeContextToJson[Json](implicit encoder: JsonLDEncoder[Json]): EncodeText[ActiveContext, Coeval] = {
    import encoder.baseEncoder._

    new EncodeText[ActiveContext, Coeval] {
      def encode(implicit activeContext: ActiveContext): ActiveContext => Coeval[String] =
        (activeContext: ActiveContext) =>
          Coeval {
            Map(
              types.`@context` -> encoder
                .fromActiveContext(activeContext)
                .getOrElse(Map[String, Json]().asJson)).asJson.noSpaces
        }
    }
  }

  implicit val encodeStringText = new EncodeText[String, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: String) => Coeval { value }
  }
  implicit val encodeBooleanText = new EncodeText[Boolean, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => Coeval { value.toString }
  }
  implicit val encodeIntText = new EncodeText[Int, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => Coeval { value.toString }
  }
  implicit val encodeDoubleText = new EncodeText[Double, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => Coeval { value.toString }
  }
  implicit val encodeLongText = new EncodeText[Long, Coeval] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => Coeval { value.toString }
  }
}
