package lspace.encode

trait EncodeText[A] extends Encode[A] {
  val encode: A => String
}
object EncodeText {
  implicit val encodeStringText = new EncodeText[String] {
    val encode = (json: String) => json
  }
}
