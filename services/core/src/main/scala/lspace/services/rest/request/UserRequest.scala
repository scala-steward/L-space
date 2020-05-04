package lspace.services.rest.request

import lspace.services.rest.security.UserSseSession

case class UserRequest[T](request: T, appSession: UserSseSession) extends WrappedRequest[T]
