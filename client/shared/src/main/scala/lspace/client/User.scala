package lspace.client

import java.time.Instant

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import lspace.structure.Property.default._
import lspace.structure.OntologyDef
import monix.eval.{Coeval, Task}

object User extends OntologyDef(lspace.NS.vocab.Lspace + "User", Set(), "User", "User of something") {

  object keys {
    object `lspace:User/role`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "role",
          "role",
          "A role assigned to this user",
          `@range` = Role.ontology :: Nil
        ) {}
    lazy val `lspace:User/role@Role`: TypedProperty[Node] = `lspace:User/role`.as(Role.ontology)

    object `lspace:User/manager`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "manager",
          "manager",
          "A user who can establish or revoke the sessions of this user.",
          `@range` = User.ontology :: Nil
        ) {}
    lazy val `lspace:User/manager@User`: TypedProperty[Node] = `lspace:User/manager`.as(User.ontology)

    object `lspace:name`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "name",
          "name",
          `@extends` = Property(lspace.NS.vocab.schema + "name") :: Nil
        )
    lazy val `lspace:name@String`: TypedProperty[String] = `lspace:name`.as(DataType.default.`@string`)

    object `sioc:last_activity_date`
        extends PropertyDef(lspace.NS.vocab.sioc + "last_activity_date", "last_activity_date")
    lazy val `sioc:last_activity_date@Instant`: TypedProperty[Instant] =
      `sioc:last_activity_date`.as(DataType.default.`@datetime`)

    object `lspace:User/status` extends PropertyDef(lspace.NS.vocab.Lspace + "User/status", "status")
    lazy val `lspace/User/status@String`: TypedProperty[String] =
      `lspace:User/status`.as(DataType.default.`@string`)
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

  def apply(iri: String, role: Set[Role] = Set(), manager: Set[User] = Set()): User = {
    val iri0     = iri
    val role0    = role
    val manager0 = manager
    new User {
      val iri = iri0
      role ++ role0
      manager ++ manager0
    }
  }

  implicit def toNode(user: User): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(typed.iriUrlString, user.iri)
      _ <- Task.parSequenceUnordered(
        user
          .role()
          .map(role =>
            DetachedGraph.nodes.upsert(role.iri).flatMap(role => node.addOut(keys.`lspace:User/role@Role`, role))
          )
      )
      _ <- Task.parSequenceUnordered(
        user
          .manager()
          .map(manager =>
            DetachedGraph.nodes
              .upsert(manager.iri)
              .flatMap(manager => node.addOut(keys.`lspace:User/manager@User`, manager))
          )
      )
    } yield node
  def toUser(node: Node): Task[User] =
    for {
      user <- Task.now(new User {
        val iri = node.iri
      })
      _ <- for {
        roles0    <- Task.parSequence(node.out(Client.keys.`lspace:Client/role@Role`).map(Role.toRole)).map(_.toSet)
        managers0 <- Task.parSequence(node.out(Client.keys.`lspace:Client/manager@User`).map(User.toUser)).map(_.toSet)
      } yield {
        user.role ++ roles0
        user.manager ++ managers0
      }
    } yield user
}

trait User extends IriResource {
  implicit def toNode: Task[Node] = this

  protected var rolesList: Coeval[Set[Role]] = Coeval.now(Set[Role]()).memoizeOnSuccess
  object role {
    def apply(): Set[Role]               = rolesList()
    def apply(iri: String): Option[Role] = rolesList().find(_.iri == iri)
    def +(role: Role): this.type = this.synchronized {
      rolesList = rolesList.map(_ + role).memoizeOnSuccess
      this
    }
    def ++(roles: Iterable[Role]): this.type = this.synchronized {
      rolesList = rolesList.map(_ ++ roles).memoizeOnSuccess
      this
    }
    def -(role: Role): this.type = this.synchronized {
      rolesList = rolesList.map(_ - role).memoizeOnSuccess
      this
    }
    def --(roles: Iterable[Role]): this.type = this.synchronized {
      rolesList = rolesList.map(_ -- roles).memoizeOnSuccess
      this
    }
  }

  protected var managersList: Coeval[Set[User]] = Coeval.now(Set[User]()).memoizeOnSuccess
  object manager {
    def apply(): Set[User]               = managersList()
    def apply(iri: String): Option[User] = managersList().find(_.iri == iri)
    def +(manager: User): this.type = this.synchronized {
      managersList = managersList.map(_ + manager).memoizeOnSuccess
      this
    }
    def ++(managers: Iterable[User]): this.type = this.synchronized {
      managersList = managersList.map(_ ++ managers).memoizeOnSuccess
      this
    }
    def -(manager: User): this.type = this.synchronized {
      managersList = managersList.map(_ - manager).memoizeOnSuccess
      this
    }
    def --(managers: Iterable[User]): this.type = this.synchronized {
      managersList = managersList.map(_ -- managers).memoizeOnSuccess
      this
    }
  }
}
