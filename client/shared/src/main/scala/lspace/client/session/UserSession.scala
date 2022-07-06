package lspace.client.session

import java.time.Instant

import lspace.client.{Client, User}
import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure.OntologyDef
import lspace.structure._
import lspace.structure.Property.default._
import monix.eval.Task

object UserSession
    extends OntologyDef(
      lspace.NS.vocab.Lspace + "UserSession",
      Set(),
      "UserSession",
      "An user session is to secure a series of requests during a limited " +
        "period of time and tied to a client and user.",
      ClientSession.ontology :: Nil
    ) {

  object keys extends ClientSession.Properties {
    object `lspace:UserSession/user`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "user",
          "user",
          "The user the session belongs to",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:UserSession/user@User`: TypedProperty[Node] = `lspace:UserSession/user`.as(Client.ontology)
  }
  override lazy val properties: List[Property] = keys.`lspace:UserSession/user` :: ClientSession.properties
  trait Properties extends ClientSession.Properties {
    val `lspace:UserSession/user`      = keys.`lspace:UserSession/user`
    val `lspace:UserSession/user@User` = keys.`lspace:UserSession/user@User`
  }

  def apply(
    iri: String,
    expiration: Instant,
    startTime: Instant,
    client: Client,
    user: User,
    endTime: Option[Instant] = None
  ): UserSession = {
    val iri0        = iri
    val expiration0 = expiration
    val startTime0  = startTime
    val endTime0    = endTime
    val client0     = client
    val user0       = user
    new UserSession {
      def iri: String              = iri0
      def expiration: Instant      = expiration0
      def startTime: Instant       = startTime0
      def endTime: Option[Instant] = endTime0
      def client: Client           = client0
      def user: User               = user0
    }
  }

  implicit def toNode(session: UserSession): Task[Node] =
    for {
      node   <- DetachedGraph.nodes.create(ontology)
      _      <- node.addOut(typed.iriUrlString, session.iri)
      _      <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, session.expiration)
      _      <- node.addOut(OpenSession.keys.`lspace:OpenSession/startTime@Instant`, session.startTime)
      client <- DetachedGraph.nodes.upsert(session.iri)
      _      <- node.addOut(keys.`lspace:ClientSession/client@Client`, client)
      user   <- DetachedGraph.nodes.upsert(session.iri)
      _      <- node.addOut(keys.`lspace:UserSession/user@User`, user)
      _      <- session.endTime.map(node.addOut(keys.`lspace:OpenSession/endTime@Instant`, _)).getOrElse(Task.unit)
    } yield node

  def toUserSession(node: Node): Task[UserSession] = {
    val expiration0: Instant =
      node
        .out(OpenSession.keys.`lspace:OpenSession/expiration@Instant`)
        .headOption
        .getOrElse(throw new Exception("no expiration date"))
    val startTime0: Instant =
      node
        .out(OpenSession.keys.`lspace:OpenSession/startTime@Instant`)
        .headOption
        .getOrElse(throw new Exception("no startTime date"))
    val endTime0: Option[Instant] = node.out(OpenSession.keys.`lspace:OpenSession/endTime@Instant`).headOption
    for {
      client0 <- node
        .out(ClientSession.keys.`lspace:ClientSession/client@Client`)
        .headOption
        .map(Client.toClient)
        .getOrElse(Task.raiseError(new Exception("no client")))
      user0 <- node
        .out(UserSession.keys.`lspace:UserSession/user@User`)
        .headOption
        .map(User.toUser)
        .getOrElse(Task.raiseError(new Exception("no user")))
    } yield new UserSession {
      val iri        = node.iri
      def expiration = expiration0
      def startTime  = startTime0
      def endTime    = endTime0
      def client     = client0
      def user       = user0
    }
  }
}

trait UserSession extends ClientSession {
  override def toNode: Task[Node] = this

  def user: User
}
