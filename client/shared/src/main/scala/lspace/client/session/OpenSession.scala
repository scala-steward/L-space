package lspace.client.session

import java.time.Instant

import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal.TypedKey
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure.Ontology.OntologyDef
import lspace.librarian.structure._

object OpenSession
    extends OntologyDef(
      lspace.NS.vocab.Lspace + "OpenSession",
      Set(),
      "OpenSession",
      "An open session is to secure a series of requests during a " +
        "limited period of time and is not bound to a client or user.",
      () => Session.ontology :: Nil
    ) {

  def apply(iri: String, expiration: Instant, startTime: Instant): OpenSession = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    node.addOut(keys.`lspace:OpenSession/expiration@Instant`, expiration)
    node.addOut(keys.`lspace:OpenSession/startTime@Instant`, startTime)
    new OpenSession(node) {}
  }

  def wrap(node: Node): OpenSession = node match {
    case node: OpenSession => node
    case _                 => new OpenSession(node) {}
  }

  object keys extends Session.Properties {
    object `lspace:OpenSession/expiration`
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "expiration",
          "expiration",
          "Date and time at which the session expires.",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:OpenSession/expiration@Instant`
      : TypedProperty[Instant] = `lspace:OpenSession/expiration` + DataType.default.`@datetime`

    object `lspace:OpenSession/startTime`
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "startTime",
          "startTime",
          "Date and time at which the session has started.",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:OpenSession/startTime@Instant`
      : TypedProperty[Instant] = `lspace:OpenSession/startTime` + DataType.default.`@datetime`

    object `lspace:OpenSession/endTime`
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "endTime",
          "endTime",
          "Date and time at which the session has ended.",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:OpenSession/endTime@Instant`
      : TypedProperty[Instant] = `lspace:OpenSession/endTime` + DataType.default.`@datetime`

  }

  override lazy val properties
    : List[Property] = keys.`lspace:OpenSession/expiration`.property :: keys.`lspace:OpenSession/startTime`.property :: keys.`lspace:OpenSession/endTime`.property :: Session.properties

  trait Properties extends Session.Properties {
    lazy val `lspace:OpenSession/expiration`: Property                  = keys.`lspace:OpenSession/expiration`
    lazy val `lspace:OpenSession/expiration@Instant`: TypedKey[Instant] = keys.`lspace:OpenSession/expiration@Instant`
    lazy val `lspace:OpenSession/startTime`: Property                   = keys.`lspace:OpenSession/startTime`
    lazy val `lspace:OpenSession/startTime@Instant`: TypedKey[Instant]  = keys.`lspace:OpenSession/startTime@Instant`
    lazy val `lspace:OpenSession/endTime`: Property                     = keys.`lspace:OpenSession/endTime`
    lazy val `lspace:OpenSession/endTime@Instant`: TypedKey[Instant]    = keys.`lspace:OpenSession/endTime@Instant`
  }
}

abstract class OpenSession(node: Node) extends WrappedNode(node) with Session {
  def expiration: Instant =
    out(OpenSession.keys.`lspace:OpenSession/expiration@Instant`).headOption
      .getOrElse(throw new Exception("no expiration date"))
  def startTime: Instant =
    out(OpenSession.keys.`lspace:OpenSession/startTime@Instant`).headOption
      .getOrElse(throw new Exception("no startTime date"))
  def endTime: Option[Instant] = out(OpenSession.keys.`lspace:OpenSession/endTime@Instant`).headOption
}
