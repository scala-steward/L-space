package lspace.parse.util

import lspace.encode.EncodeException

case class ToJsonException(message: String) extends EncodeException(message)
