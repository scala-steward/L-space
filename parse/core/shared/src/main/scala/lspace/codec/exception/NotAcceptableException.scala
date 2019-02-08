package lspace.codec.exception

case class NotAcceptableException(message: String) extends DecodeException(message)
