package lspace.services.rest.endpoints

import io.finch._

case class AuthApi() extends Api {

  private val path = "session"

  /**
    * session-iri for id-provider to acknowledge
    */
  private val create = get("create") { Ok("some-iri") }

  /**
    * acknowledge ending to user
    */
  private val drop = delete("drop") { Ok("acknowledge ending to user") }

  /**
    * path for identity provider to confirm user identity for some session
    */
  private val vouch = post("vouch") { Ok("user-iri, session-iri") }

  val api = path :: (create :+: drop :+: vouch)
}
