package lspace.services.rest.endpoints

import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.http.Response
import com.twitter.io.{Buf, Reader}
import io.finch._
import cats.effect.IO
import com.twitter.finagle.Http
import lspace.services.app.JsApp

case class AppService(apps: List[JsApp]) extends Service {

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

//  import io.finch.catsEffect._
//  val p: Endpoint[IO, Int] = param[Int]("foo")
//  p.map { x => x }

  val apia: Endpoint[IO, String] = get("hello" :: paths[String]) { segments: Seq[String] =>
    Ok("Hello, World!")
  }
  Http.server.serve(":8080", apia.toServiceAs[Text.Plain])

  val static: Endpoint[IO, AsyncStream[Buf]] = get("assets" :: paths[String]) { segments: Seq[String] =>
    val path = segments.mkString("/")
    Ok(
      AsyncStream.fromFuture(
        Reader
          .readAll(Reader.fromStream(getClass.getResourceAsStream(s"/public/$path")))
          .map { buf =>
            buf
          })).withHeader(getContentType(path))
  }

  val api = apps
    .map { app =>
      get(app.id) { Ok(htmlResponse(app.rendered)) }
    }
    .reduce(_.coproduct(_)) :+: static
}
