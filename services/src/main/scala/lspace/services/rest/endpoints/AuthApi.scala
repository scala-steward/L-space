package lspace.services.rest.endpoints

import io.finch._
import lspace.services.rest.security.SessionBroker
import shapeless._

trait AuthApi extends Api {

  def sessionBroker: SessionBroker
  def path = "session"

  /**
    * session-iri for id-provider to acknowledge
    */
  def create = post(pathEmpty) { Ok("some-iri") }
  def info = get(path[String]) { iri: String =>
    Ok(iri)
  }

  /**
    * acknowledge ending to user
    */
  def drop = delete(pathEmpty) { Ok("acknowledge ending to user") }

  /**
    * path for identity provider to confirm user identity for some session
    */
  def vouch = post(path[String] :: path("vouch")).map {
    case id: String => Ok(s"$id ${sessionBroker.get(id).map(_.toString).getOrElse("NO SESSION")}")
  }

  def api = path :: (create :+: drop :+: vouch)
}
