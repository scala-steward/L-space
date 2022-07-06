package lspace.services.rest.endpoints

import cats.Applicative
import cats.effect.IO
import com.twitter.finagle.http.Cookie
import com.twitter.util.Duration
import io.finch._
import lspace.services.crypto.Crypto
import lspace.services.rest.endpoints.util.ValidateCookie
import lspace.services.rest.security.{ClientSseSession, OpenSseSession, SessionBroker, UserSseSession}
import monix.execution.Scheduler
import shapeless.{:+:, ::, CNil, HNil}

object AuthApi {
  def apply(sessionBroker: SessionBroker, crypto: Crypto)(implicit scheduler: Scheduler): AuthApi =
    new AuthApi(sessionBroker, crypto, sessionBroker.baseIri.stripPrefix("https://").stripPrefix("http://"))(scheduler)
  def apply(sessionBroker: SessionBroker, crypto: Crypto, cookieId: String)(implicit scheduler: Scheduler): AuthApi =
    new AuthApi(
      sessionBroker,
      crypto,
      if (cookieId.nonEmpty) cookieId.stripPrefix("https://").stripPrefix("http://")
      else sessionBroker.baseIri.stripPrefix("https://").stripPrefix("http://")
    )(scheduler)
}
class AuthApi(sessionBroker: SessionBroker, crypto: Crypto, cookieId: String)(implicit scheduler: Scheduler)
    extends Api {

  val path = "session"

  def authenticatedFilter[F[_]](implicit F: Applicative[F]) =
    ValidateCookie(
      cookieId,
      (coded: String) => {
        val iri = crypto.decryptBase64ToUTF8String(coded)
        sessionBroker.getOpenSseSession(iri).isDefined
      }
    )
  def authenticated: Endpoint[IO, OpenSseSession] =
    cookie(cookieId)
      .mapOutput { encryptedCookie =>
        val iri = crypto.decryptBase64ToUTF8String(encryptedCookie.value)
        sessionBroker.getOpenSseSession(iri) match {
          case Some(session) => Ok(session)
          case None          => Unauthorized(new Exception(s"No valid session"))
        }
      }
      .handle { case e: Error.NotPresent =>
        Unauthorized(e)
      }

  def authenticatedClientFilter[F[_]](implicit F: Applicative[F]) =
    ValidateCookie(
      cookieId,
      (coded: String) => {
        val iri = crypto.decryptBase64ToUTF8String(coded)
        sessionBroker.getClientSseSession(iri).isDefined
      }
    )
  def authenticatedClient: Endpoint[IO, ClientSseSession] =
    cookie(cookieId)
      .mapOutput { encryptedCookie =>
        val iri = crypto.decryptBase64ToUTF8String(encryptedCookie.value)
        sessionBroker.getClientSseSession(iri) match {
          case Some(session) => Ok(session)
          case None          => Unauthorized(new Exception(s"No valid session"))
        }
      }
      .handle { case e: Error.NotPresent =>
        Unauthorized(e)
      }
  def authenticatedUserFilter[F[_]](implicit F: Applicative[F]) =
    ValidateCookie(
      cookieId,
      (coded: String) => {
        val iri = crypto.decryptBase64ToUTF8String(coded)
        sessionBroker.getUserSseSession(iri).isDefined
      }
    )
  def authenticatedUser: Endpoint[IO, UserSseSession] =
    cookie(cookieId)
      .mapOutput { encryptedCookie =>
        val iri = crypto.decryptBase64ToUTF8String(encryptedCookie.value)
        sessionBroker.getUserSseSession(iri) match {
          case Some(session) => Ok(session)
          case None          => Unauthorized(new Exception(s"No valid session"))
        }
      }
      .handle { case e: Error.NotPresent =>
        Unauthorized(e)
      }

  /** Create a new session
    * @return
    */
  def create: Endpoint[IO, String] = post(pathEmpty).mapOutputAsync { case u =>
    (for {
      session <- sessionBroker.create()
    } yield Ok(session.session.iri)
      .withCookie(
        new Cookie(
          cookieId,
          crypto.encryptToBase64(session.session.iri),
          maxAge = Some(Duration.fromSeconds(14400)),
          httpOnly = true
        )
      )
      .withHeader("Location" -> session.session.iri)).to[IO]
  }

  /** View session info
    * @return
    */
  def info: Endpoint[IO, String] = get(path[String] :: authenticated).mapOutput { case iri :: session :: HNil =>
    if (session.iri == sessionBroker.baseIri + "/session/" + iri) Ok(iri)
    else Unauthorized(new Exception("Can only view own resources"))
  }

  /** Drop session
    * @return
    */
  def drop: Endpoint[IO, Unit :+: Unit :+: CNil] =
    delete(pathEmpty :: authenticated).mapOutput { case session =>
      sessionBroker.drop(session.iri)
      NoContent[Unit]
    } :+: delete(path[String] :: authenticated).mapOutput { case iri :: session :: HNil =>
      // TODO: validate if drop action is allowed
      sessionBroker.drop(sessionBroker.baseIri + "/session/" + iri)
      NoContent[Unit]
    }

  /** path for identity provider to confirm user identity for some session
    */
  def vouch = post(path[String] :: path("vouch")).map { case id: String =>
    Ok(s"$id ${sessionBroker.get(id).map(_.toString).getOrElse("NO SESSION")}")
  }

  def api = path :: (create :+: drop :+: vouch)
}
