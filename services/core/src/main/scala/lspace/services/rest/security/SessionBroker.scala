package lspace.services.rest.security

import java.util.concurrent.ConcurrentHashMap

import lspace.client.session.Session
import monix.eval.Task

import scala.collection.concurrent
import scala.collection.JavaConverters._

object SessionBroker {
  def apply(baseIri: String): SessionBroker = new SessionBroker(baseIri)
}
class SessionBroker(val baseIri: String) {

  private lazy val sessionCache: concurrent.Map[String, Session] =
    new ConcurrentHashMap[String, Session]().asScala

  def get(iri: String): Option[Session] = sessionCache.get(iri)
  def getOpenSseSession(iri: String): Option[OpenSseSession] = get(iri) collect {
    case session: OpenSseSession => session
  }
  def getClientSseSession(iri: String): Option[ClientSseSession] = get(iri) collect {
    case session: ClientSseSession => session
  }
  def getUserSseSession(iri: String): Option[UserSseSession] = get(iri) collect {
    case session: UserSseSession => session
  }

  def create(): Task[OpenSseSession] =
    for {
      session <- OpenSseSession(
        baseIri + "/session/" + java.util.UUID.randomUUID().toString + "-" + scala.math.random())
      _ = sessionCache.put(session.iri, session)
    } yield session
  def drop(iri: String): Unit = sessionCache.remove(iri)
}
