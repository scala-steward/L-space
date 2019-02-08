package lspace.decode

abstract class DecodeException(message: String) extends Exception(message)
abstract class NotAcceptable(message: String)   extends DecodeException(message)
