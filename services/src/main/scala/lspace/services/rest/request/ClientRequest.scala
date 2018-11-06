package lspace.rest.play.request

import com.twitter.finagle.http.Request
import lspace.services.rest.security.ClientSseSession

case class ClientRequest(request: Request, appSession: ClientSseSession) extends WrappedRequest
