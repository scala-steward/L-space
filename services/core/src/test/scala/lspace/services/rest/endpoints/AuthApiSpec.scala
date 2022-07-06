package lspace.services.rest.endpoints

import cats.effect.IO
import com.twitter.finagle.http.{Request, Response, Status}
import io.finch._
import lspace.services.LApplication
import lspace.services.crypto.Crypto
import lspace.services.rest.security.SessionBroker
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, Matchers}
import shapeless.{:+:, CNil}
import lspace.services.util._

class AuthApiSpec extends AsyncWordSpec with Matchers with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  override def executionContext = lspace.Implicits.Scheduler.global

  implicit val authApi = AuthApi(SessionBroker("https://example.org"), Crypto.AES(Crypto.AES.generateSecretKey(256)))

  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[Text.Plain :+: CNil](authApi.create :+: authApi.info :+: authApi.drop)
    .toService

  "a auth-api" should {
    "create an opensession" in {
      def input: Input =
        Input
          .post("/")
      (for {
        cookiesAndIri <-
          Task
            .from(authApi.create(input).output.get)
            .map { output =>
              output.status shouldBe Status.Ok
              (output.cookies, output.value)
            }
        (cookies, iri) = cookiesAndIri._1 -> cookiesAndIri._2
        _ <- Task
          .deferFuture {
            val input = Input
              .get(s"/${iri.reverse.takeWhile(_ != '/').reverse}")
            input.request.cookies.addAll(cookies)
            service(input.request)
          }
          .map { r =>
            r.status shouldBe Status.Ok
          }
        _ <- Task
          .deferFuture {
            val input = Input
              .delete(s"/${iri.reverse.takeWhile(_ != '/').reverse}")
            input.request.cookies.addAll(cookies)
            service(input.request)
          }
          .map { r =>
            r.status shouldBe Status.NoContent
          }
        _ <- Task
          .deferFuture {
            val input = Input
              .get(s"/${iri.reverse.takeWhile(_ != '/').reverse}")
            input.request.cookies.addAll(cookies)
            service(input.request)
          }
          .map { r =>
            r.status shouldBe Status.Unauthorized
          }
      } yield succeed).runToFuture
    }
  }
}
