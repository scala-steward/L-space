package lspace.codec.exception

case class FromJsonException(message: String)       extends DecodeException(message)
case class NotAcceptableException(message: String)  extends DecodeException(message)
case class UnexpectedJsonException(message: String) extends DecodeException(message)
