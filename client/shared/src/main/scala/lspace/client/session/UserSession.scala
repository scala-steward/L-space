package lspace.client.session

import java.time.Instant

import lspace.client.{Client, User}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._
import lspace.types._

object UserSession {
  protected val ontologyNode =
    MemGraphDefault.ns.upsertNode(s"https://data.l-space.eu/schema/UserSession")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.EXTENDS --> ClientSession.ontology
  ontologyNode --- Property.default.label --> "UserSession" --- Property.default.language --> "en"
  ontologyNode --- Property.default.comment --> "An user session is to secure a series of requests during a limited period of time and tied to an identified client and identified user." --- Property.default.language --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, expiration: Instant, startTime: Instant, client: Client, user: User): UserSession = {
    val node = DetachedGraph.createNode(ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    node.addOut(OpenSession.keys.expirationDate, expiration)
    node.addOut(OpenSession.keys.startTime, startTime)
    node.addOut(ClientSession.keys.clientClient, client)
    node.addOut(keys.userUser, user)
    new UserSession(node) {}
  }

  def wrap(node: Node): UserSession = node match {
    case node: UserSession => node
    case _                 => new UserSession(node) {}
  }

  object keys {
    private val userNode = MemGraphDefault.ns.upsertNode(s"${ontology.iri}/User")
    userNode.addLabel(Property.ontology)
    userNode --- Property.default.label --> "User" --- Property.default.language --> "en"
    userNode --- Property.default.comment --> "The user the session belongs to" --- Property.default.language --> "en"
    userNode --- Property.default.range --> User.ontology

    lazy val user: Property           = Property(userNode)
    val userUser: TypedProperty[Node] = user + User.ontology
  }

  ontologyNode --- Property.default.properties --> keys.user
}

abstract class UserSession(node: Node) extends ClientSession(node) {
  def user: User =
    out(UserSession.keys.userUser).headOption
      .map(User.wrap)
      .getOrElse(throw new Exception("no user"))
}
