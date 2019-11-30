package lspace.services.rest.endpoints

import cats.effect._
import cats.effect.IO._
import lspace.services.app.JsApp

import scala.concurrent.Future

trait AppApi[Req, Res] extends Api[Req, Res] {

  def app: JsApp

  private def htmlResponse(document: String): Res = {
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

  implicit private val S: ContextShift[IO] = IO.contextShift(lspace.Implicits.Scheduler.global)
  val static: Endpoint[IO, _root_.fs2.Stream[IO, Buf]] = get("assets" :: paths[String]) { segments: Seq[String] =>
    val path = segments.mkString("/")
    import lspace.services.util.twFutureToScala
    Ok(
      _root_.fs2.Stream.eval(
        IO.fromFuture(IO(Reader
          .readAll(Reader.fromStream(getClass.getResourceAsStream(s"/public/$path")))
          .map { buf =>
            buf
          }: Future[Buf])))).withHeader(getContentType(path))
  }

  val api = (MatchHeaderContains[IO]("Accept", "text/html") :: get(pathEmpty) { Ok(htmlResponse(app.rendered)) }) :+: static
}
