package lspace.codec.exception

case class ToJsonException(message: String) extends EncodeException(message)
