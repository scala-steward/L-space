package lspace.client.session

import java.time.Instant

import lspace.client.Client
import lspace.librarian.datatype.DataType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure.Ontology.OntologyDef
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._

object ClientSession
    extends OntologyDef(
      lspace.NS.vocab.Lspace + "ClientSession",
      Set(),
      "ClientSession",
      "An client session is to secure a series of requests during a " +
        "limited period of time and bound to a client.",
      () => OpenSession.ontology :: Nil
    ) {

  def apply(iri: String, expiration: Instant, startTime: Instant, client: Client): ClientSession = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
    node.addOut(OpenSession.keys.`lspace:OpenSession/expiration@Instant`, expiration)
    node.addOut(OpenSession.keys.`lspace:OpenSession/startTime@Instant`, startTime)
    node.addOut(keys.`lspace:ClientSession/client@Client`, client)
    new ClientSession(node) {}
  }

  def wrap(node: Node): ClientSession = node match {
    case node: ClientSession => node
    case _                   => new ClientSession(node) {}
  }

  object keys extends OpenSession.Properties {
    object `lspace:ClientSession/client`
        extends Property.PropertyDef(
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
}

abstract class ClientSession(node: Node) extends OpenSession(node) {
  def client: Client =
    out(ClientSession.keys.`lspace:ClientSession/client@Client`).headOption
      .map(Client.wrap)
      .getOrElse(throw new Exception("no client"))
}
