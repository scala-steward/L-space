package lspace.services

import com.twitter.finagle.http.{Request, Response}

object LService {}
trait LService {
  def service: com.twitter.finagle.Service[Request, Response]
}
