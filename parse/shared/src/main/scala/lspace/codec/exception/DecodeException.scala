package lspace.codec.exception

abstract class DecodeException(message: String) extends Exception(message)
abstract class NotAcceptable(message: String)   extends DecodeException(message)
