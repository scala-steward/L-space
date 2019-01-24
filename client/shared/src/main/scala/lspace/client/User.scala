package lspace.client

import java.time.Instant

import lspace.librarian.datatype.DataType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.librarian.structure.OntologyDef

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
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "role",
          "role",
          "A role assigned to this user",
          `@range` = () => Role.ontology :: Nil
        ) {}
    lazy val `lspace:User/role@Role`: TypedProperty[Node] = `lspace:User/role` + Role.ontology

    object `lspace:User/manager`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "manager",
          "manager",
          "A user who can establish or revoke the sessions of this user.",
          `@range` = () => User.ontology :: Nil
        ) {}
    lazy val `lspace:User/manager@User`: TypedProperty[Node] = `lspace:User/manager` + User.ontology

    object `lspace:name`
        extends PropertyDef(lspace.NS.vocab.Lspace + "name",
                            "name",
                            `@extends` = () => Property(lspace.NS.vocab.schema + "name") :: Nil)
    lazy val `lspace:name@String`: TypedProperty[String] = `lspace:name` + DataType.default.`@string`

    object `sioc:last_activity_date`
        extends PropertyDef(lspace.NS.vocab.sioc + "last_activity_date", "last_activity_date")
    lazy val `sioc:last_activity_date@Instant`: TypedProperty[Instant] =
      `sioc:last_activity_date` + DataType.default.`@datetime`

    object `lspace:User/status` extends PropertyDef(lspace.NS.vocab.Lspace + "User/status", "status")
    lazy val `lspace/User/status@String`: TypedProperty[String] =
      `lspace:User/status` + DataType.default.`@string`
  }

  override lazy val properties
    : List[Property] = keys.`lspace:User/role`.property :: keys.`lspace:User/manager`.property ::
    keys.`lspace:name`.property :: keys.`sioc:last_activity_date`.property :: keys.`lspace:User/status`.property :: Nil

  trait Properties {
    val `lspace:User/role`: Property = keys.`lspace:User/role`
    val `lspace:User/role@Role`      = keys.`lspace:User/role@Role`

    val `lspace:User/manager`: Property = keys.`lspace:User/manager`
    val `lspace:User/manager@User`      = keys.`lspace:User/manager@User`

    lazy val `lspace:name`: Property                     = keys.`lspace:name`
    lazy val `lspace:name@String`: TypedProperty[String] = keys.`lspace:name@String`

    lazy val `sioc:last_activity_date`: Property                       = keys.`sioc:last_activity_date`
    lazy val `sioc:last_activity_date@Instant`: TypedProperty[Instant] = keys.`sioc:last_activity_date@Instant`

    lazy val `lspace:User/status`: Property                     = keys.`lspace:User/status`
    lazy val `lspace/User/status@String`: TypedProperty[String] = keys.`lspace/User/status@String`
  }
}

case class User private (node: Node) extends WrappedNode(node) {
  def role: Set[Role]    = out(User.keys.`lspace:User/role@Role`).map(Role.wrap).toSet
  def manager: Set[User] = out(User.keys.`lspace:User/manager@User`).map(User.wrap).toSet
}
