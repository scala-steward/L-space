package lspace.client.session

import java.time.Instant

import lspace.datatype.DataType
import lspace.librarian.traversal.TypedKey
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure.OntologyDef
import lspace.structure._
import monix.eval.Task

object OpenSession
    extends OntologyDef(
      lspace.NS.vocab.Lspace + "OpenSession",
      Set(),
      "OpenSession",
      "An open session is to secure a series of requests during a " +
        "limited period of time and is not bound to a client or user.",
      Session.ontology :: Nil
    ) {

  object keys extends Session.Properties {
    object `lspace:OpenSession/expiration`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "expiration",
          "expiration",
          "Date and time at which the session expires.",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:OpenSession/expiration@Instant`: TypedProperty[Instant] =
      `lspace:OpenSession/expiration`.as(DataType.default.`@datetime`)

    object `lspace:OpenSession/startTime`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "startTime",
          "startTime",
          "Date and time at which the session has started.",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:OpenSession/startTime@Instant`: TypedProperty[Instant] =
      `lspace:OpenSession/startTime`.as(DataType.default.`@datetime`)

    object `lspace:OpenSession/endTime`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "endTime",
          "endTime",
          "Date and time at which the session has ended.",
          `@range` = DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:OpenSession/endTime@Instant`: TypedProperty[Instant] =
      `lspace:OpenSession/endTime`.as(DataType.default.`@datetime`)

  }

  override lazy val properties: List[Property] =
    keys.`lspace:OpenSession/expiration`.property :: keys.`lspace:OpenSession/startTime`.property :: keys.`lspace:OpenSession/endTime`.property :: Session.properties

  trait Properties extends Session.Properties {
    lazy val `lspace:OpenSession/expiration`: Property                  = keys.`lspace:OpenSession/expiration`
    lazy val `lspace:OpenSession/expiration@Instant`: TypedKey[Instant] = keys.`lspace:OpenSession/expiration@Instant`
    lazy val `lspace:OpenSession/startTime`: Property                   = keys.`lspace:OpenSession/startTime`
    lazy val `lspace:OpenSession/startTime@Instant`: TypedKey[Instant]  = keys.`lspace:OpenSession/startTime@Instant`
    lazy val `lspace:OpenSession/endTime`: Property                     = keys.`lspace:OpenSession/endTime`
    lazy val `lspace:OpenSession/endTime@Instant`: TypedKey[Instant]    = keys.`lspace:OpenSession/endTime@Instant`
  }

  def apply(iri: String, expiration: Instant, startTime: Instant, endTime: Option[Instant] = None): OpenSession = {
    val iri0        = iri
    val expiration0 = expiration
    val startTime0  = startTime
    val endTime0    = endTime
    new OpenSession {
      def iri: String              = iri0
      def expiration: Instant      = expiration0
      def startTime: Instant       = startTime0
      def endTime: Option[Instant] = endTime0
    }
  }

  implicit def toNode(session: OpenSession): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(Property.default.typed.iriUrlString, session.iri)
      _    <- node.addOut(keys.`lspace:OpenSession/expiration@Instant`, session.expiration)
      _    <- node.addOut(keys.`lspace:OpenSession/startTime@Instant`, session.startTime)
      _    <- session.endTime.map(node.addOut(keys.`lspace:OpenSession/endTime@Instant`, _)).getOrElse(Task.unit)
    } yield node

  def toOpenSession(node: Node): Task[OpenSession] = Task {
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
    new OpenSession {
      val iri        = node.iri
      def expiration = expiration0
      def startTime  = startTime0
      def endTime    = endTime0
    }
  }
}

trait OpenSession extends Session {
  implicit def toNode: Task[Node] = this

  def expiration: Instant
  def startTime: Instant
  def endTime: Option[Instant]
}
