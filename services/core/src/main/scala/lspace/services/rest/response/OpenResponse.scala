package lspace.services.rest.response

import com.twitter.finagle.http.Response
import lspace.services.rest.security.OpenSseSession

case class OpenResponse[T](response: T, appSession: OpenSseSession) extends WrappedResponse[T]
