package lspace.client.session

import java.time.Instant

import lspace.client.Client
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._

object ClientSession {
  protected val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(s"https://data.l-space.eu/schema/ClientSession")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@extends` --> OpenSession.ontology
  ontologyNode --- Property.default.`@label` --> "ClientSession" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "An client session is to secure a series of requests during a limited period of time and tied to an identified client." --- Property.default.`@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, expiration: Instant, startTime: Instant, client: Client): ClientSession = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(Property.default.typed.iriUrlString, iri)
    node.addOut(OpenSession.keys.expirationDate, expiration)
    node.addOut(OpenSession.keys.startTime, startTime)
    node.addOut(keys.clientClient, client)
    new ClientSession(node) {}
  }

  def wrap(node: Node): ClientSession = node match {
    case node: ClientSession => node
    case _                   => new ClientSession(node) {}
  }

  object keys {
    private val clientNode = MemGraphDefault.ns.nodes.upsert(s"${ontology.iri}/Client")
    clientNode.addLabel(Property.ontology)
    clientNode --- Property.default.`@label` --> "Client" --- Property.default.`@language` --> "en"
    clientNode --- Property.default.`@comment` --> "The device the session is bound to." --- Property.default.`@language` --> "en"
    clientNode --- Property.default.`@range` --> Client.ontology

    lazy val client: Property             = Property(clientNode)
    val clientClient: TypedProperty[Node] = client + Client.ontology
  }

  ontologyNode --- Property.default.`@properties` --> keys.client
}

abstract class ClientSession(node: Node) extends OpenSession(node) {
  def client: Client =
    out(ClientSession.keys.clientClient).headOption
      .map(Client.wrap)
      .getOrElse(throw new Exception("no client"))
}
