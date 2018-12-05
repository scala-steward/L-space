package lspace.client

import lspace.client.session.ClientSession
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault

object Client {
  protected val ontologyNode = MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "Client")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- `@label` --> "Client" --- `@language` --> "en"
  ontologyNode --- `@comment` --> "A client ..." --- `@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, role: Set[Role], manager: Set[User], session: Set[ClientSession]): Client = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
    role.foreach(role => node.addOut(keys.roleRole, role))
    manager.foreach(manager => node.addOut(keys.managerRole, manager))
    session.foreach(session => node.addOut(keys.sessionClientSession, session))
    new Client(node)
  }

  def wrap(node: Node): Client = node match {
    case node: Client => node
    case _            => Client(node)
  }

  object keys {
    private val roleNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/role")
    roleNode.addLabel(Property.ontology)
    roleNode --- `@label` --> "Role" --- `@language` --> "en"
    roleNode --- `@comment` --> "A role assigned to this user" --- `@language` --> "en"
    roleNode --- `@container` --> types.`@set`
    roleNode --- `@range` --> Role.ontology

    lazy val role: Property           = Property(roleNode)
    val roleRole: TypedProperty[Node] = role + Role.ontology

    private val managerNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/manager")
    managerNode.addLabel(Property.ontology)
    managerNode --- `@label` --> "Manager" --- `@language` --> "en"
    managerNode --- `@comment` --> "A user who can establish or revoke the sessions of this user." --- `@language` --> "en"
    managerNode --- `@container` --> types.`@set`
    managerNode --- `@range` --> User.ontology

    lazy val manager: Property           = Property(managerNode)
    val managerRole: TypedProperty[Node] = manager + User.ontology

    private val sessionNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/session")
    sessionNode.addLabel(Property.ontology)
    sessionNode --- `@label` --> "session" --- `@language` --> "en"
    sessionNode --- `@comment` --> "A session ..." --- `@language` --> "en"
    sessionNode --- `@container` --> types.`@set`
    sessionNode --- `@range` --> ClientSession.ontology

    lazy val session: Property                    = Property(sessionNode)
    val sessionClientSession: TypedProperty[Node] = session + ClientSession.ontology
  }

  ontologyNode --- `@properties` --> keys.role
  ontologyNode --- `@properties` --> keys.manager
  ontologyNode --- `@properties` --> keys.session
}

case class Client private (node: Node) extends WrappedNode(node) {
  def role: Set[Role]    = this.out(Client.keys.roleRole).map(Role.wrap).toSet
  def manager: Set[User] = this.out(Client.keys.managerRole).map(User.wrap).toSet
  def session: Set[ClientSession] =
    this.out(Client.keys.sessionClientSession).map(ClientSession.wrap).toSet
}
