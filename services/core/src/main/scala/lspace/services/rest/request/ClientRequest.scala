package lspace.services.rest.request

import lspace.services.rest.security.ClientSseSession

case class ClientRequest[T](request: T, appSession: ClientSseSession) extends WrappedRequest[T]
