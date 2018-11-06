package lspace.client

import lspace.client.session.ClientSession
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault

object Client {
  protected val ontologyNode = MemGraphDefault.ns.upsertNode(s"https://data.l-space.eu/schema/Client")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.label --> "Client" --- Property.default.language --> "en"
  ontologyNode --- Property.default.comment --> "A client ..." --- Property.default.language --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, role: Set[Role], manager: Set[User], session: Set[ClientSession]): Client = {
    val node = DetachedGraph.createNode(ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
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
    private val roleNode = MemGraphDefault.ns.upsertNode(s"${ontology.iri}/role")
    roleNode.addLabel(Property.ontology)
    roleNode --- Property.default.label --> "Role" --- Property.default.language --> "en"
    roleNode --- Property.default.comment --> "A role assigned to this user" --- Property.default.language --> "en"
    roleNode --- Property.default.container --> types.set
    roleNode --- Property.default.range --> Role.ontology

    lazy val role: Property           = Property(roleNode)
    val roleRole: TypedProperty[Node] = role + Role.ontology

    private val managerNode = MemGraphDefault.ns.upsertNode(s"${ontology.iri}/manager")
    managerNode.addLabel(Property.ontology)
    managerNode --- Property.default.label --> "Manager" --- Property.default.language --> "en"
    managerNode --- Property.default.comment --> "A user who can establish or revoke the sessions of this user." --- Property.default.language --> "en"
    managerNode --- Property.default.container --> types.set
    managerNode --- Property.default.range --> User.ontology

    lazy val manager: Property           = Property(managerNode)
    val managerRole: TypedProperty[Node] = manager + User.ontology

    private val sessionNode = MemGraphDefault.ns.upsertNode(s"${ontology.iri}/session")
    sessionNode.addLabel(Property.ontology)
    sessionNode --- Property.default.label --> "session" --- Property.default.language --> "en"
    sessionNode --- Property.default.comment --> "A session ..." --- Property.default.language --> "en"
    sessionNode --- Property.default.container --> types.set
    sessionNode --- Property.default.range --> ClientSession.ontology

    lazy val session: Property                    = Property(sessionNode)
    val sessionClientSession: TypedProperty[Node] = session + ClientSession.ontology
  }

  ontologyNode --- Property.default.properties --> keys.role
  ontologyNode --- Property.default.properties --> keys.manager
  ontologyNode --- Property.default.properties --> keys.session
}

case class Client private (node: Node) extends WrappedNode(node) {
  def role: Set[Role]    = this.out(Client.keys.roleRole).map(Role.wrap).toSet
  def manager: Set[User] = this.out(Client.keys.managerRole).map(User.wrap).toSet
  def session: Set[ClientSession] =
    this.out(Client.keys.sessionClientSession).map(ClientSession.wrap).toSet
}
