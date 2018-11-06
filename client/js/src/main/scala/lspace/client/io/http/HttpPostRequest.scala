package lspace.client.io.http

import org.scalajs.dom

object HttpPostRequest {
  def create(url: String, headers: Map[String, String], data: String): HttpPostRequest = {
    val req = new dom.XMLHttpRequest()
    req.open("POST", url)
    req.responseType = ""
    req.timeout = 0
    req.withCredentials = true
    headers.foreach(x => req.setRequestHeader(x._1, x._2))
    new HttpPostRequest(url, data, req)
  }
}

class HttpPostRequest(val url: String, val data: String, val domReq: dom.XMLHttpRequest) extends HttpRequest {
  private val datahash         = data.hashCode
  private val headerHash       = domReq.getAllResponseHeaders().hashCode
  private val hash             = datahash + headerHash
  override def hashCode(): Int = hash

  override def equals(o: scala.Any): Boolean = o match {
    case request: HttpPostRequest => request.url == url && request.hashCode() == hashCode()
    case _                        => false
  }
}
