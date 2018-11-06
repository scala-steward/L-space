package lspace.parse.util

import lspace.librarian.structure.Resource

import scala.util.Try

object HttpClientImpl extends HttpClient {
  def getResource[T](iri: String)(parse: String => T): Try[T] = Try {
    //    println(s"http get resource ${iri}")
    val jsonIri = if (iri.endsWith(".jsonld")) iri else iri + ".jsonld"
    parse(scala.io.Source.fromURL(jsonIri).mkString)
  }
}
