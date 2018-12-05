package lspace.client

import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault

object User {
  protected val ontologyNode = MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "User")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- `@label` --> "User" --- `@language` --> "en"
  ontologyNode --- `@comment` --> "User of something" --- `@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, role: Set[Role], manager: Set[User]): User = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
    role.foreach(role => node.addOut(keys.roleRole, role))
    manager.foreach(manager => node.addOut(keys.managerUser, manager))
    new User(node)
  }

  def wrap(node: Node): User = node match {
    case node: User => node
    case _          => User(node)
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
    val managerUser: TypedProperty[Node] = manager + User.ontology
  }

  ontologyNode --- `@properties` --> keys.role
  ontologyNode --- `@properties` --> keys.manager
}

case class User private (node: Node) extends WrappedNode(node) {
  def role: Set[Role]    = out(User.keys.roleRole).map(Role.wrap).toSet
  def manager: Set[User] = out(User.keys.managerUser).map(User.wrap).toSet
}
