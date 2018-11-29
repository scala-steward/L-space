package lspace.services.rest.security

import java.time.{Instant, LocalDateTime, ZoneId}

import lspace.client.session._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure.Property

object OpenSseSession {
  def apply(
      iri: String,
      expiration: Instant = LocalDateTime.now.plusHours(4).atZone(ZoneId.systemDefault).toInstant): OpenSseSession = {
    val node = DetachedGraph.nodes.create(OpenSession.ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    node.addOut(OpenSession.keys.expirationDate, expiration)

    new OpenSseSession(new OpenSession(node) {}) with WithSse
  }
}

class OpenSseSession(val session: OpenSession) extends WithSse
