package lspace.services.rest.response

import lspace.services.rest.security.UserSseSession

case class UserResponse[T](response: T, appSession: UserSseSession) extends WrappedResponse[T]
