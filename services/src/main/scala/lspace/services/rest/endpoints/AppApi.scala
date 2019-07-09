package lspace.services.rest.endpoints

import com.twitter.finagle.http.Response
import com.twitter.io.{Buf, Reader}
import io.finch._
import cats.effect._
import cats.effect.IO._
import com.twitter.finagle.Http
import lspace.services.app.JsApp

class AppApi(app: JsApp) extends Api {

  private def htmlResponse(document: String): Response = {
    val rep = Response()
    rep.content = Buf.Utf8(document)
    rep.contentType = "text/html"
    rep
  }

  //  def reader(path: String): Reader = Reader.fromFile(new File(path))
  //  val asset: Endpoint[Buf] = get("public" :: path[String]) { path: String =>
  //    Reader.readAll(reader(path)).map(Ok)
  //  }

  def getContentType(assetPath: String): (String, String) = {
    val contentType = if (assetPath.endsWith(".js")) {
      "application/javascript"
    } else if (assetPath.endsWith(".css")) {
      "text/css"
    } else {
      "text/plain"
    }
    "Content-Type" -> contentType
  }

  import com.twitter.util.{Future => TwFuture}

  import scala.concurrent.{Future => ScFuture, Promise => ScPromise}
  implicit def twFutureToScala[T](twFuture: TwFuture[T]): ScFuture[T] = {
    val prom = ScPromise[T]()
    twFuture.onSuccess { res: T =>
      prom.success(res)
    }
    twFuture.onFailure { t: Throwable =>
      prom.failure(t)
    }
    prom.future
  }
  val static: Endpoint[IO, _root_.fs2.Stream[IO, Buf]] = get("assets" :: paths[String]) { segments: Seq[String] =>
    val path = segments.mkString("/")

    Ok(
      _root_.fs2.Stream.eval(
        IO.fromFuture(IO(Reader
          .readAll(Reader.fromStream(getClass.getResourceAsStream(s"/public/$path")))
          .map { buf =>
            buf
          }: ScFuture[Buf])))).withHeader(getContentType(path))
  }

  val api = get(pathEmpty) { Ok(htmlResponse(app.rendered)) } :+: static
}
