package lspace.client.session

import java.time.Instant

import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types._

object OpenSession {
  protected val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "OpenSession")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@label` --> "OpenSession" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "An open session is to secure a series of requests during a limited period of time and is not bound to a client or user." --- Property.default.`@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, expiration: Instant, startTime: Instant): OpenSession = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    node.addOut(keys.expirationDate, expiration)
    node.addOut(keys.startTimeDate, startTime)
    new OpenSession(node) {}
  }

  def wrap(node: Node): OpenSession = node match {
    case node: OpenSession => node
    case _                 => new OpenSession(node) {}
  }

  object keys {
    private val expirationNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/Expiration")
    expirationNode.addLabel(Property.ontology)
    expirationNode --- Property.default.`@label` --> "Expiration" --- Property.default.`@language` --> "en"
    expirationNode --- Property.default.`@comment` --> "Date and time at which the session expires." --- Property.default.`@language` --> "en"
    expirationNode --- Property.default.`@range` --> DataType.default.`@datetime`

    lazy val expiration: Property              = Property(expirationNode)
    val expirationDate: TypedProperty[Instant] = expiration + DataType.default.`@datetime`

    private val startTimeNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/StartTime")
    startTimeNode.addLabel(Property.ontology)
    startTimeNode --- Property.default.`@label` --> "StartTime" --- Property.default.`@language` --> "en"
    startTimeNode --- Property.default.`@comment` --> "Date and time at which the session has started." --- Property.default.`@language` --> "en"
    startTimeNode --- Property.default.`@range` --> DataType.default.`@datetime`

    lazy val startTime: Property              = Property(startTimeNode)
    val startTimeDate: TypedProperty[Instant] = startTime + DataType.default.`@datetime`

    private val endTimeNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/EndTime")
    endTimeNode.addLabel(Property.ontology)
    endTimeNode --- Property.default.`@label` --> "EndTime" --- Property.default.`@language` --> "en"
    endTimeNode --- Property.default.`@comment` --> "Date and time at which the session has ended." --- Property.default.`@language` --> "en"
    endTimeNode --- Property.default.`@range` --> DataType.default.`@datetime`

    lazy val endTime: Property              = Property(endTimeNode)
    val endTimeDate: TypedProperty[Instant] = endTime + DataType.default.`@datetime`
  }

  ontologyNode --- Property.default.`@properties` --> keys.expiration
  ontologyNode --- Property.default.`@properties` --> keys.startTime
  ontologyNode --- Property.default.`@properties` --> keys.endTime
}

trait Session extends IriResource

abstract class OpenSession(node: Node) extends WrappedNode(node) with Session {
  def expiration: Instant =
    out(OpenSession.keys.expirationDate).headOption
      .getOrElse(throw new Exception("no expiration date"))
  def startTime: Instant =
    out(OpenSession.keys.startTimeDate).headOption
      .getOrElse(throw new Exception("no startTime date"))
  def endTime: Option[Instant] = out(OpenSession.keys.endTimeDate).headOption
}
