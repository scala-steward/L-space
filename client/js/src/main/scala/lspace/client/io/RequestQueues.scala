package lspace.client.io

import lspace.client.io.http.HttpResponse
import lspace.client.io.http.{HttpGetRequest, HttpPostRequest, HttpRequest}
import org.scalajs.dom

import scala.collection.mutable

/**
  * Queue http requests, merge equal request and return observables
  * Created by thijs on 28-11-16.
  */
class RequestQueues {
  val domains = mutable.Set[String]()
  val queues  = mutable.HashMap[String, mutable.Queue[HttpRequest]]()

  private def findOrCreate(domain: String) = {
    queues.get(domain) match {
      case Some(q) => q
      case None =>
        val newQueue = (domain, mutable.Queue[HttpRequest]())
        queues += newQueue
        newQueue._2
    }
  }

  def enqueue[T <: HttpRequest](request: T)(callback: HttpResponse => Any): T = { //TODO: (onerror: dom.XMLHttpRequest => Unit)
    val localportPattern = "[:][0-9]+".r
    val uri              = request.url.stripPrefix("https://").stripPrefix("http://")
    val domain = localportPattern
      .findFirstIn(uri)
      .map { port =>
        val uriparts = localportPattern.split(uri)
        uriparts(0) + port
      }
      .getOrElse { request.url.stripPrefix("https://").stripPrefix("http://").takeWhile(_ != '/') } //was "/"

    val q = findOrCreate(domain)
    val r = q.find(_ == request) match {
      case Some(request) =>
        request match {
          case r: HttpGetRequest =>
            dom.console.log("append callback: " + r.url + " " + r.data.getOrElse(""))
          case r: HttpPostRequest =>
            dom.console.log("append callback: " + r.url + " " + r.data)
        }
        request.callbacks += callback
        request
      case None =>
        //TODO: if(request in Cache) callback(cached result) else ..
        //        request.callbacks += callback
        request.callbacks += callback
        q += request
        request
    }
    domains += domain
    r.asInstanceOf[T]
  }

  def dequeue(domain: String): HttpRequest = {
    queues(domain).dequeue()
  }
}
