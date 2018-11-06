package lspace.rest.play.request

import com.twitter.finagle.http.Request
import lspace.services.rest.security.UserSseSession

case class UserRequest(request: Request, appSession: UserSseSession) extends WrappedRequest
