package lspace.client

import lspace.client.session.ClientSession
import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import lspace.structure.Property.default._
import lspace.structure.OntologyDef
import monix.eval.{Coeval, Task}

object Client extends OntologyDef(lspace.NS.vocab.Lspace + "Client", Set(), "Client", "A client ..") {

  object keys {
    object `lspace:Client/role`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "Role",
          "Role",
          "A role assigned to this user",
          `@range` = Role.ontology :: Nil
        ) {}
    lazy val `lspace:Client/role@Role`: TypedProperty[Node] = `lspace:Client/role` as Role.ontology

    object `lspace:Client/manager`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "Manager",
          "Manager",
          "A user who can establish or revoke the sessions of this user.",
          `@range` = User.ontology :: Nil
        ) {}
    lazy val `lspace:Client/manager@User`: TypedProperty[Node] = `lspace:Client/manager` as User.ontology

    object `lspace:Client/session`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "Client/session",
          "session",
          "A session ...",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:Client/session@ClientSession`
      : TypedProperty[Node] = `lspace:Client/session` as ClientSession.ontology
  }
  override lazy val properties
    : List[Property] = keys.`lspace:Client/role`.property :: keys.`lspace:Client/manager`.property :: keys.`lspace:Client/session`.property :: Nil
  trait Properties {
    val `lspace:Client/role`: Property        = keys.`lspace:Client/role`
    val `lspace:Client/role@Role`             = keys.`lspace:Client/role@Role`
    val `lspace:Client/manager`: Property     = keys.`lspace:Client/manager`
    val `lspace:Client/manager@User`          = keys.`lspace:Client/manager@User`
    val `lspace:Client/session`: Property     = keys.`lspace:Client/session`
    val `lspace:Client/session@ClientSession` = keys.`lspace:Client/session@ClientSession`
  }

  def apply(iri: String,
            role: Set[Role] = Set(),
            manager: Set[User] = Set(),
            session: Set[ClientSession] = Set()): Client = {
    val iri0     = iri
    val role0    = role
    val manager0 = manager
    val session0 = session
    new Client {
      val iri = iri0
      role ++ role0
      manager ++ manager0
      session ++ session0
    }
  }

  implicit def toNode(client: Client): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(typed.iriUrlString, client.iri)
      _ <- Task.gatherUnordered(
        client
          .role()
          .map(role =>
            DetachedGraph.nodes.upsert(role.iri).flatMap(role => node.addOut(keys.`lspace:Client/role@Role`, role))))
      _ <- Task.gatherUnordered(
        client
          .manager()
          .map(
            manager =>
              DetachedGraph.nodes
                .upsert(manager.iri)
                .flatMap(manager => node.addOut(keys.`lspace:Client/manager@User`, manager))))
      _ <- Task.gatherUnordered(
        client
          .session()
          .map(
            session =>
              DetachedGraph.nodes
                .upsert(session.iri)
                .flatMap(session => node.addOut(keys.`lspace:Client/session@ClientSession`, session))))
    } yield node

  def toClient(node: Node): Task[Client] =
    for {
      client <- Task.now(new Client {
        val iri = node.iri
      })
      _ <- for {
        roles0    <- Task.gather(node.out(Client.keys.`lspace:Client/role@Role`).map(Role.toRole)).map(_.toSet)
        managers0 <- Task.gather(node.out(Client.keys.`lspace:Client/manager@User`).map(User.toUser)).map(_.toSet)
        sessions0 <- Task
          .gather(node.out(Client.keys.`lspace:Client/session@ClientSession`).map(ClientSession.toClientSession))
          .map(_.toSet)
      } yield {
        client.role ++ roles0
        client.manager ++ managers0
        client.session ++ sessions0
      }
    } yield client
}

trait Client extends IriResource {
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

  protected var sessionsList: Coeval[Set[ClientSession]] = Coeval.now(Set[ClientSession]()).memoizeOnSuccess
  object session {
    def apply(): Set[ClientSession]               = sessionsList()
    def apply(iri: String): Option[ClientSession] = sessionsList().find(_.iri == iri)
    def +(session: ClientSession): this.type = this.synchronized {
      sessionsList = sessionsList.map(_ + session).memoizeOnSuccess
      this
    }
    def ++(sessions: Iterable[ClientSession]): this.type = this.synchronized {
      sessionsList = sessionsList.map(_ ++ sessions).memoizeOnSuccess
      this
    }
    def -(session: ClientSession): this.type = this.synchronized {
      sessionsList = sessionsList.map(_ - session).memoizeOnSuccess
      this
    }
    def --(sessions: Iterable[ClientSession]): this.type = this.synchronized {
      sessionsList = sessionsList.map(_ -- sessions).memoizeOnSuccess
      this
    }
  }
}
