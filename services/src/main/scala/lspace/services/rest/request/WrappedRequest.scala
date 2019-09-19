package lspace.services.rest.request

import com.twitter.finagle.http.{Request, Response}

trait WrappedRequest {
  def request: Request
}
