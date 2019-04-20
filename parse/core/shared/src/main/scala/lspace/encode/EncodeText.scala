package lspace.encode

import lspace.codec.{ActiveContext, ContextedT}

trait EncodeText[A] extends Encode[A] {
  def encode(implicit activeContext: ActiveContext): A => String
}
object EncodeText {

  implicit def contextedTToJsonLD[T](implicit en: EncodeText[T]) = new EncodeText[ContextedT[T]] {
    def encode(implicit activeContext: ActiveContext) =
      (ct: ContextedT[T]) => en.encode(activeContext ++ ct.activeContext)(ct.t)
  }

  implicit val encodeStringText = new EncodeText[String] {
    def encode(implicit activeContext: ActiveContext) = (value: String) => value
  }
  implicit val encodeBooleanText = new EncodeText[Boolean] {
    def encode(implicit activeContext: ActiveContext) = (value: Boolean) => value.toString
  }
  implicit val encodeIntText = new EncodeText[Int] {
    def encode(implicit activeContext: ActiveContext) = (value: Int) => value.toString
  }
  implicit val encodeDoubleText = new EncodeText[Double] {
    def encode(implicit activeContext: ActiveContext) = (value: Double) => value.toString
  }
  implicit val encodeLongText = new EncodeText[Long] {
    def encode(implicit activeContext: ActiveContext) = (value: Long) => value.toString
  }
}
