package lspace.client.io.http

import lspace.client.io.http.HttpRequestBase
import org.scalajs.dom

trait HttpRequest extends HttpRequestBase {
  val domReq: dom.XMLHttpRequest

  def abort(): Unit = {
    domReq.abort()
  }

  def status: Int = domReq.status
}
