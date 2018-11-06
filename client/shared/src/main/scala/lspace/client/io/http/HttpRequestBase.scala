package lspace.client.io.http

import scala.collection.mutable.ListBuffer

trait HttpRequestBase {
  def url: String
  val callbacks: ListBuffer[(HttpResponse) => Any] = ListBuffer[(HttpResponse) => Any]()

  def abort(): Unit

  def status: Int
}
