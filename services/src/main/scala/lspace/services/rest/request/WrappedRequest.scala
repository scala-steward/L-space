package lspace.rest.play.request

import com.twitter.finagle.http.Request

trait WrappedRequest {
  def request: Request
}
