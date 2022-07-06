package lspace.services.rest.security

import java.time.{Instant, LocalDateTime, ZoneId}

import lspace.Label
import lspace.client.{Client, User}
import lspace.client.session.{ClientSession, OpenSession, UserSession}
import lspace.provider.detached.DetachedGraph
import monix.eval.Task

object UserSseSession {
  def apply(
    iri: String,
    client: Client,
    user: User,
    expiration: Instant = LocalDateTime.now.plusHours(4).atZone(ZoneId.systemDefault).toInstant
  ): Task[UserSseSession] =
    for {
      node        <- DetachedGraph.nodes.create(UserSession.ontology)
      _           <- node.addOut(Label.P.typed.iriUrlString, iri)
      _           <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, expiration)
      _           <- node.addOut(OpenSession.keys.`lspace:OpenSession/startTime@Instant`, Instant.now())
      clientNode  <- client.toNode
      _           <- node.addOut(ClientSession.keys.`lspace:ClientSession/client@Client`, clientNode)
      userNode    <- user.toNode
      _           <- node.addOut(UserSession.keys.`lspace:UserSession/user@User`, userNode)
      userSession <- UserSession.toUserSession(node)
    } yield new UserSseSession(userSession)
}

class UserSseSession(override val session: UserSession) extends ClientSseSession(session) with UserSession {
  override def user: User = session.user
}
