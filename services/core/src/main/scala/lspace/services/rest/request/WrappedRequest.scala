package lspace.services.rest.request

import com.twitter.finagle.http.{Request, Response}

trait WrappedRequest[T] {
  def request: T
}
