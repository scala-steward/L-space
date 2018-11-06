package lspace.services.rest.security

import java.time.{Instant, LocalDateTime, ZoneId}

import lspace.client.Client
import lspace.client.session.{ClientSession, OpenSession}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure.Property

object ClientSseSession {
  def apply(
      iri: String,
      client: Client,
      expiration: Instant = LocalDateTime.now.plusHours(4).atZone(ZoneId.systemDefault).toInstant): ClientSseSession = {
    val node = DetachedGraph.createNode(OpenSession.ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    node.addOut(OpenSession.keys.expirationDate, expiration)
    node.addOut(ClientSession.keys.clientClient, client)

    new ClientSseSession(new ClientSession(node) {})
  }
}

class ClientSseSession(override val session: ClientSession) extends OpenSseSession(session)
