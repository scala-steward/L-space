package lspace.client.io

import java.util.NoSuchElementException

import monix.reactive.subjects.Var
import lspace.client.io.http.HttpResponse
import lspace.client.io.http.{HttpGetRequest, HttpPostRequest, HttpRequest}
import org.scalajs
import org.scalajs.dom
import org.scalajs.dom.ext.KeyValue

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Random

object RequestHandler {

  import monix.execution.Scheduler.Implicits.global

  private val queues = new RequestQueues()

  private var startMoreRequests = Var(false)

  def suspend = {
    startMoreRequests := false
  }

  def run = {
    //    if (dom.window.navigator.onLine) {
    startMoreRequests := true
    //    } else if (Main.controller.loginName.now.isDefined) startMoreRequests.update(true) //FIXME: fix for single-machine setup
  }

  object Method extends Enumeration {
    case class Method(name: String) extends AnyVal
    val GET    = Method("GET")
    val POST   = Method("POST")
    val PUT    = Method("PUT")
    val PATCH  = Method("PATCH")
    val DELETE = Method("DELETE")
    val OPTION = Method("OPTION")
    val HEAD   = Method("HEAD")
  }

  private val requestsInProgress = mutable.Set[HttpRequest]()

  val maxConcurrency = 20

  def nudge = {
    if (requestsInProgress.size < maxConcurrency) run
  }

  private val random = new Random()
  startMoreRequests.foreach { b =>
    //    dom.console.log("startMoreRequests")
    if (b) {
      //      dom.console.log("true")
      while (requestsInProgress.size < maxConcurrency && queues.domains.nonEmpty) {
        val domain = queues.domains.toVector(random.nextInt(queues.domains.size))
        try {
          val request = queues.dequeue(domain)
          //          scalajs.dom.console.log("execute request: " + request.url)
          //          dom.console.log(request.domReq.readyState)
          if (request.domReq.readyState == 1) {
            requestsInProgress += request
            val domReq = request.domReq
            domReq.onreadystatechange = { (e: dom.Event) =>
              if (domReq.readyState == 4) {
                if ((domReq.status >= 200 && domReq.status < 300) || domReq.status == 304) {
                  //                  dom.console.log("complete: " + request.url)
                  requestsInProgress.remove(request)
                  request.callbacks.foreach { f =>
                    Future {
                      f(HttpResponse(domReq.status, domReq.responseText))
                    }
                  }
                  if (!startMoreRequests.firstL.coeval.value.fold(x => false, b => b))
                    startMoreRequests := true
                } //          promise.success(req)
                else dom.console.error(domReq.status + " : " + request.url)
                //          promise.failure(AjaxException(req))
              }
            }
            request match {
              case r: HttpGetRequest =>
                //              get(domain, r)
                r.data match {
                  case Some(data) => domReq.send(data)
                  case None       => domReq.send()
                }
              case r: HttpPostRequest =>
                //              post(domain, r)
                domReq.send(r.data)
              case _ =>
                scalajs.dom.console
                  .log("RequestHandler mismatch: " + request.url) //Non matching will fill InProgress queue indefinitely, FIX!
            }
          } else requestsInProgress.remove(request)
        } catch {
          case t: NoSuchElementException => queues.domains.remove(domain)
        }
      }
      startMoreRequests := false
    }
  }

  def newRequest[T <: HttpRequest](request: T)(callback: HttpResponse => Any) = {
    queues.enqueue(request)(callback)
    nudge
  }

  //  def withRangeHeaders(range: (Option[Int], Option[Int])) =
  //    Map("Range" -> (range._1.getOrElse(0) + "-" + range._2.getOrElse(50))) ++ this.headers

  def get(request: HttpGetRequest)(callback: HttpResponse => Any): HttpGetRequest = {
    //TODO: check http cache and return if cached

    val newR = queues.enqueue(request)(callback)
    //    dom.console.log("enqueued")
    nudge
    //    dom.console.log("nudged")
    newR
  }

  def getAsync(request: HttpGetRequest): (HttpGetRequest, Future[HttpResponse]) = {
    val p = Promise[HttpResponse]()
    val newR = get(request) { response =>
      p success response
    }
    nudge
    newR -> p.future
  }

  def post(request: HttpPostRequest)(callback: HttpResponse => Any): HttpPostRequest = {
    val reqR = queues.enqueue(request)(callback)
    nudge
    reqR
  }

  def postAsync(request: HttpPostRequest): (HttpPostRequest, Future[HttpResponse]) = {
    val p = Promise[HttpResponse]()
    val reqR = post(request) { response =>
      p success response
    }
    nudge
    reqR -> p.future
  }
}
