package lspace.services.rest.endpoints

import io.finch._

trait AuthApi extends Api {

  def path = "session"

  /**
    * session-iri for id-provider to acknowledge
    */
  def create = get("create") { Ok("some-iri") }

  /**
    * acknowledge ending to user
    */
  def drop = delete("drop") { Ok("acknowledge ending to user") }

  /**
    * path for identity provider to confirm user identity for some session
    */
  def vouch = post("vouch") { Ok("user-iri, session-iri") }

  def api = path :: (create :+: drop :+: vouch)
}
