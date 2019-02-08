package lspace.codec.exception

case class UnexpectedJsonException(message: String) extends DecodeException(message)
