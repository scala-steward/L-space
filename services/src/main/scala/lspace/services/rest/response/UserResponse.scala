package lspace.services.rest.response

import com.twitter.finagle.http.Response
import lspace.services.rest.security.UserSseSession

case class UserResponse(response: Response, appSession: UserSseSession) extends WrappedResponse
