package lspace.services.rest.response

import com.twitter.finagle.http.Response
import lspace.services.rest.security.ClientSseSession

case class ClientResponse[T](response: T, appSession: ClientSseSession) extends WrappedResponse[T]
