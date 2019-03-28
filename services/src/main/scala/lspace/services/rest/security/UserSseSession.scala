package lspace.services.rest.security

import java.time.{Instant, LocalDateTime, ZoneId}

import lspace.Label
import lspace.client.{Client, User}
import lspace.client.session.{ClientSession, OpenSession, UserSession}
import lspace.provider.detached.DetachedGraph
import lspace.structure.Property
import monix.eval.Task

object UserSseSession {
  def apply(iri: String,
            client: Client,
            user: User,
            expiration: Instant = LocalDateTime.now.plusHours(4).atZone(ZoneId.systemDefault).toInstant)
    : Task[UserSseSession] = {

    for {
      node <- DetachedGraph.nodes.create(UserSession.ontology)
      _    <- node.addOut(Label.P.typed.iriUrlString, iri)
      _    <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, expiration)
      _    <- node.addOut(ClientSession.keys.`lspace:ClientSession/client@Client`, client)
      _    <- node.addOut(UserSession.keys.`lspace:UserSession/user@User`, user)
    } yield new UserSseSession(new UserSession(node) {})
  }
}

class UserSseSession(override val session: UserSession) extends OpenSseSession(session)
