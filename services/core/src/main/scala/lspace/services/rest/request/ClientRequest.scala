package lspace.services.rest.request

import com.twitter.finagle.http.{Request, Response}
import lspace.services.rest.response.WrappedResponse
import lspace.services.rest.security.ClientSseSession

case class ClientRequest[T](request: T, appSession: ClientSseSession) extends WrappedRequest[T]
