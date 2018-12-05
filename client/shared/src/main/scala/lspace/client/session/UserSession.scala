package lspace.client.session

import java.time.Instant

import lspace.client.{Client, User}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.types._

object UserSession {
  protected val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "UserSession")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- `@extends` --> ClientSession.ontology
  ontologyNode --- `@label` --> "UserSession" --- `@language` --> "en"
  ontologyNode --- `@comment` --> "An user session is to secure a series of requests during a limited period of time and tied to a client and user." --- `@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, expiration: Instant, startTime: Instant, client: Client, user: User): UserSession = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
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
    private val userNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/User")
    userNode.addLabel(Property.ontology)
    userNode --- `@label` --> "User" --- `@language` --> "en"
    userNode --- `@comment` --> "The user the session belongs to" --- `@language` --> "en"
    userNode --- `@range` --> User.ontology

    lazy val user: Property           = Property(userNode)
    val userUser: TypedProperty[Node] = user + User.ontology
  }

  ontologyNode --- `@properties` --> keys.user
}

abstract class UserSession(node: Node) extends ClientSession(node) {
  def user: User =
    out(UserSession.keys.userUser).headOption
      .map(User.wrap)
      .getOrElse(throw new Exception("no user"))
}
