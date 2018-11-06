package lspace.client.io.http

import lspace.client.io.http.HttpRequestBase

trait HttpRequest extends HttpRequestBase {

  def abort(): Unit

  def status: Int
}
