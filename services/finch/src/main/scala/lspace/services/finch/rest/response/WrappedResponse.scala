package lspace.services.rest.response

import com.twitter.finagle.http.Response

trait WrappedResponse {
  def response: Response
}
