package lspace.codec.exception

case class FromJsonException(message: String) extends DecodeException(message)
