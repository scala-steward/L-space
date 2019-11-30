package lspace.services.rest.request

import com.twitter.finagle.http.{Request, Response}
import lspace.services.rest.response.WrappedResponse
import lspace.services.rest.security.UserSseSession

case class UserRequest(request: Request, appSession: UserSseSession) extends WrappedRequest
