package lspace.client.io.http

import org.scalajs.dom

object HttpGetRequest {
  def create(url: String, headers: Map[String, String], data: Option[String] = None): HttpGetRequest = {
    val req = new dom.XMLHttpRequest()
    req.open("GET", url)
    req.responseType = ""
    req.timeout = 0
    req.withCredentials = true
    headers.foreach(x => req.setRequestHeader(x._1, x._2))
    new HttpGetRequest(url, data, req)
  }
}

class HttpGetRequest(val url: String, val data: Option[String] = None, val domReq: dom.XMLHttpRequest)
    extends HttpRequest {
  private val datahash         = data.hashCode
  private val headerHash       = domReq.getAllResponseHeaders().hashCode
  private val hash             = datahash + headerHash
  override def hashCode(): Int = hash

  override def equals(o: scala.Any): Boolean = o match {
    case request: HttpGetRequest => request.url == url && request.hashCode() == hashCode()
    case _                       => false
  }
}
