package lspace.services.rest.request

import com.twitter.finagle.http.{Request, Response}
import lspace.services.rest.response.WrappedResponse
import lspace.services.rest.security.OpenSseSession

case class OpenRequest(request: Request, appSession: OpenSseSession) extends WrappedRequest
