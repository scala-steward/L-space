package lspace.services.rest.security

import java.util.concurrent.ConcurrentHashMap

import lspace.client.session.Session

import scala.collection.concurrent
import scala.collection.JavaConverters._

object SessionBroker {
  def apply(): SessionBroker = new SessionBroker()
}
class SessionBroker {

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

  def add(session: Session): Unit = sessionCache.put(session.iri, session)
  def drop(iri: String): Unit     = sessionCache.remove(iri)
}
