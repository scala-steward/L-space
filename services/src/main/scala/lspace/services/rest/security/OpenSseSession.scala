package lspace.services.rest.security

import java.time.{Instant, LocalDateTime, ZoneId}

import lspace.Label
import lspace.client.session._
import lspace.provider.detached.DetachedGraph
import lspace.structure.Property
import monix.eval.Task

object OpenSseSession {
  def apply(iri: String, expiration: Instant = LocalDateTime.now.plusHours(4).atZone(ZoneId.systemDefault).toInstant)
    : Task[OpenSseSession] = {

    for {
      node        <- DetachedGraph.nodes.create(OpenSession.ontology)
      _           <- node.addOut(Label.P.typed.iriUrlString, iri)
      _           <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, expiration)
      openSession <- OpenSession.toOpenSession(node)
    } yield new OpenSseSession(openSession) with WithSse
  }
}

class OpenSseSession(val session: OpenSession) extends WithSse
