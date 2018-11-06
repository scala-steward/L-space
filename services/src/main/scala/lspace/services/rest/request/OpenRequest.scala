package lspace.rest.play.request

import com.twitter.finagle.http.Request
import lspace.services.rest.security.OpenSseSession

case class OpenRequest(request: Request, appSession: OpenSseSession) extends WrappedRequest
