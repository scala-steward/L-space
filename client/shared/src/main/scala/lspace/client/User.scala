package lspace.client

import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault

object User {
  protected val ontologyNode = MemGraphDefault.ns.upsertNode(s"https://data.l-space.eu/schema/User")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.label --> "User" --- Property.default.language --> "en"
  ontologyNode --- Property.default.comment --> "User of something" --- Property.default.language --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, role: Set[Role], manager: Set[User]): User = {
    val node = DetachedGraph.createNode(ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    role.foreach(role => node.addOut(keys.roleRole, role))
    manager.foreach(manager => node.addOut(keys.managerRole, manager))
    new User(node)
  }

  def wrap(node: Node): User = node match {
    case node: User => node
    case _          => User(node)
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
  }

  ontologyNode --- Property.default.properties --> keys.role
  ontologyNode --- Property.default.properties --> keys.manager
}

case class User private (node: Node) extends WrappedNode(node) {
  def role: Set[Role]    = out(User.keys.roleRole).map(Role.wrap).toSet
  def manager: Set[User] = out(User.keys.managerRole).map(User.wrap).toSet
}
