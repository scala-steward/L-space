package lspace.client.session

import java.time.Instant

import lspace.client.Client
import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure.OntologyDef
import lspace.structure._
import lspace.structure.Property.default._
import monix.eval.Task

object ClientSession
    extends OntologyDef(
      lspace.NS.vocab.Lspace + "ClientSession",
      Set(),
      "ClientSession",
      "An client session is to secure a series of requests during a " +
        "limited period of time and bound to a client.",
      () => OpenSession.ontology :: Nil
    ) {

  object keys extends OpenSession.Properties {
    object `lspace:ClientSession/client`
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "client",
          "client",
          "The client (device) the session is bound to.",
          `@range` = () => DataType.default.`@datetime` :: Nil
        ) {}
    lazy val `lspace:ClientSession/client@Client`: TypedProperty[Node] = `lspace:ClientSession/client` + Client.ontology
  }
  override lazy val properties: List[Property] = keys.`lspace:ClientSession/client` :: OpenSession.properties
  trait Properties extends OpenSession.Properties {
    val `lspace:ClientSession/client`        = keys.`lspace:ClientSession/client`
    val `lspace:ClientSession/client@Client` = keys.`lspace:ClientSession/client@Client`
  }

  def apply(iri: String,
            expiration: Instant,
            startTime: Instant,
            client: Client,
            endTime: Option[Instant] = None): ClientSession = {
    val iri0        = iri
    val expiration0 = expiration
    val startTime0  = startTime
    val endTime0    = endTime
    val client0     = client
    new ClientSession {
      def iri: String              = iri0
      def expiration: Instant      = expiration0
      def startTime: Instant       = startTime0
      def endTime: Option[Instant] = endTime0
      def client: Client           = client0
    }
  }

  implicit def toNode(session: ClientSession): Task[Node] =
    for {
      node   <- DetachedGraph.nodes.create(ontology)
      _      <- node.addOut(typed.iriUrlString, session.iri)
      _      <- node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, session.expiration)
      _      <- node.addOut(OpenSession.keys.`lspace:OpenSession/startTime@Instant`, session.startTime)
      client <- DetachedGraph.nodes.upsert(session.iri)
      _      <- node.addOut(keys.`lspace:ClientSession/client@Client`, client)
      _      <- session.endTime.map(node.addOut(keys.`lspace:OpenSession/endTime@Instant`, _)).getOrElse(Task.unit)
    } yield node

  def toClientSession(node: Node): Task[ClientSession] = {
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
    } yield
      new ClientSession {
        val iri        = node.iri
        def expiration = expiration0
        def startTime  = startTime0
        def endTime    = endTime0
        def client     = client0
      }
  }
}

trait ClientSession extends OpenSession {
  override def toNode: Task[Node] = this

  def client: Client
}
