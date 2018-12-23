package lspace.client

import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.NS.types
import lspace.librarian.structure.Ontology.OntologyDef

object User extends OntologyDef(lspace.NS.vocab.Lspace + "User", Set(), "User", "User of something") {

  def apply(iri: String, role: Set[Role], manager: Set[User]): User = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
    role.foreach(role => node.addOut(keys.`lspace:User/role@Role`, role))
    manager.foreach(manager => node.addOut(keys.`lspace:User/manager@User`, manager))
    new User(node)
  }

  def wrap(node: Node): User = node match {
    case node: User => node
    case _          => User(node)
  }

  object keys {
    object `lspace:User/role`
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "role",
          "role",
          "A role assigned to this user",
          container = types.`@set` :: Nil,
          `@range` = () => Role.ontology :: Nil
        ) {}
    lazy val `lspace:User/role@Role`: TypedProperty[Node] = `lspace:User/role` + Role.ontology

    object `lspace:User/manager`
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "manager",
          "manager",
          "A user who can establish or revoke the sessions of this user.",
          container = types.`@set` :: Nil,
          `@range` = () => User.ontology :: Nil
        ) {}
    lazy val `lspace:User/manager@User`: TypedProperty[Node] = `lspace:User/manager` + User.ontology
  }
  override lazy val properties
    : List[Property] = keys.`lspace:User/role`.property :: keys.`lspace:User/manager`.property :: Nil
  trait Properties {
    val `lspace:User/role`: Property    = keys.`lspace:User/role`
    val `lspace:User/role@Role`         = keys.`lspace:User/role@Role`
    val `lspace:User/manager`: Property = keys.`lspace:User/manager`
    val `lspace:User/manager@User`      = keys.`lspace:User/manager@User`
  }
}

case class User private (node: Node) extends WrappedNode(node) {
  def role: Set[Role]    = out(User.keys.`lspace:User/role@Role`).map(Role.wrap).toSet
  def manager: Set[User] = out(User.keys.`lspace:User/manager@User`).map(User.wrap).toSet
}
