package lspace.services.rest.response

import com.twitter.finagle.http.Response
import lspace.services.rest.security.ClientSseSession

case class ClientResponse(response: Response, appSession: ClientSseSession) extends WrappedResponse
