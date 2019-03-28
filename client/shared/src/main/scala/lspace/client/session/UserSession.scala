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
      () => ClientSession.ontology :: Nil
    ) {

  def apply(iri: String, expiration: Instant, startTime: Instant, client: Client, user: User): Task[UserSession] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(typed.iriUrlString, iri)
      _    <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, expiration)
      _    <- node.addOut(OpenSession.keys.`lspace:OpenSession/startTime@Instant`, startTime)
      _    <- node.addOut(ClientSession.keys.`lspace:ClientSession/client@Client`, client)
      _    <- node.addOut(keys.`lspace:UserSession/user@User`, user)
    } yield new UserSession(node) {}
  }

  def wrap(node: Node): UserSession = node match {
    case node: UserSession => node
    case _                 => new UserSession(node) {}
  }

  object keys extends ClientSession.Properties {
    object `lspace:UserSession/user`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "user",
          "user",
          "The user the session belongs to",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:UserSession/user@User`: TypedProperty[Node] = `lspace:UserSession/user` + Client.ontology
  }
  override lazy val properties: List[Property] = keys.`lspace:UserSession/user` :: ClientSession.properties
  trait Properties extends ClientSession.Properties {
    val `lspace:UserSession/user`      = keys.`lspace:UserSession/user`
    val `lspace:UserSession/user@User` = keys.`lspace:UserSession/user@User`
  }
}

abstract class UserSession(node: Node) extends ClientSession(node) {
  def user: User =
    out(UserSession.keys.`lspace:UserSession/user@User`).headOption
      .map(User.wrap)
      .getOrElse(throw new Exception("no user"))
}
