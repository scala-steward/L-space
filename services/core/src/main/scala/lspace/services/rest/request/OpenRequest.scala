package lspace.services.rest.request

import lspace.services.rest.security.OpenSseSession

case class OpenRequest[T](request: T, appSession: OpenSseSession) extends WrappedRequest[T]
