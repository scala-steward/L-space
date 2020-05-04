package lspace.services.rest.response

import lspace.services.rest.security.OpenSseSession

case class OpenResponse[T](response: T, appSession: OpenSseSession) extends WrappedResponse[T]
