package lspace.parse.util

import lspace.librarian.structure.Resource
import org.scalajs.dom.raw.XMLHttpRequest

import scala.scalajs.js.JSON
import scala.util.Try

object HttpClientImpl extends HttpClient {

  def getResource[T](iri: String)(parse: String => T): Try[T] = Try {
    //    println(s"http get resource ${iri}")
    val jsonIri = if (iri.endsWith(".jsonld")) iri else iri + ".jsonld"
    //    parse(Await.result(dom.ext.Ajax.get(jsonIri).map(response => JSON.parse(response.responseText)), 10 seconds))
    val request = new XMLHttpRequest()
    //    request.setRequestHeader("Accept", "application/ld+json")
    request.open("GET", jsonIri, false)
    request.send(null)
    if (request.status == 200) {
      parse(request.responseText)
    } else {
      throw new Exception(s"HttpRequest error-code ${request.status} for iri $jsonIri")
    }
  }
}
