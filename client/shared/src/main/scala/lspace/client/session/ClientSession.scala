package lspace.client.session

import java.time.Instant

import lspace.client.Client
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._

object ClientSession {
  protected val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "ClientSession")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- `@extends` --> OpenSession.ontology
  ontologyNode --- `@label` --> "ClientSession" --- `@language` --> "en"
  ontologyNode --- `@comment` --> "An client session is to secure a series of requests during a limited period of time and bound to a client." --- `@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def apply(iri: String, expiration: Instant, startTime: Instant, client: Client): ClientSession = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
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
    clientNode --- `@label` --> "Client" --- `@language` --> "en"
    clientNode --- `@comment` --> "The client (device) the session is bound to." --- `@language` --> "en"
    clientNode --- `@range` --> Client.ontology

    lazy val client: Property             = Property(clientNode)
    val clientClient: TypedProperty[Node] = client + Client.ontology
  }

  ontologyNode --- `@properties` --> keys.client
}

abstract class ClientSession(node: Node) extends OpenSession(node) {
  def client: Client =
    out(ClientSession.keys.clientClient).headOption
      .map(Client.wrap)
      .getOrElse(throw new Exception("no client"))
}
