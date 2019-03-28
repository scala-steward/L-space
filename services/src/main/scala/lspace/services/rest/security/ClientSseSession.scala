package lspace.services.rest.security

import java.time.{Instant, LocalDateTime, ZoneId}

import lspace.Label
import lspace.client.Client
import lspace.client.session.{ClientSession, OpenSession}
import lspace.provider.detached.DetachedGraph
import monix.eval.Task

object ClientSseSession {
  def apply(iri: String,
            client: Client,
            expiration: Instant = LocalDateTime.now.plusHours(4).atZone(ZoneId.systemDefault).toInstant)
    : Task[ClientSseSession] = {
    for {
      node <- DetachedGraph.nodes.create(ClientSession.ontology)
      _    <- node.addOut(Label.P.typed.iriUrlString, iri)
      _    <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, expiration)
      _    <- node.addOut(ClientSession.keys.`lspace:ClientSession/client@Client`, client)
    } yield new ClientSseSession(new ClientSession(node) {})
  }
}

class ClientSseSession(override val session: ClientSession) extends OpenSseSession(session)
