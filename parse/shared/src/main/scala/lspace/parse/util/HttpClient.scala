package lspace.parse.util

import scala.util.Try

trait HttpClient {
  def getResource[T](iri: String)(parse: String => T): Try[T]
}
