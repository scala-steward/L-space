package lspace.client

import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._

object Role {
  protected val ontologyNode = MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "Role")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- `@label` --> "Role" --- `@language` --> "en"
  ontologyNode --- `@comment` --> "A role ..." --- `@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  //TODO: test implicit graph helper-functions
  implicit class WithGraph(graph: Graph) {
    def newRole(iri: String): Role = {
      val node = graph.nodes.create(ontology)
      node.addOut(typed.iriUrlString, iri)
      new Role(node)
    }
  }
  def apply(iri: String): Role = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(typed.iriUrlString, iri)
    new Role(node)
  }
  def wrap(node: Node): Role = node match {
    case node: Role => node
    case _          => Role(node)
  }

  object keys {
    //    val role = PropertyKey(s"$uri/role")
    //    role.setContainer(types.set)
    //    val roleRole: TypedPropertyKey[Node] = role.addRange(Role.ontology)
    //    role.property(PropertyKeysBootstrap.labelString, "Role").head.property(PropertyKeysBootstrap.languageString, "en")
    //    role.property(PropertyKeysBootstrap.commentString, "A role assigned to this user").head
    //      .property(PropertyKeysBootstrap.languageString, "en")
  }

  //  ontology.addPropertyKey(keys.role)

  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Role private (override val value: Node) extends WrappedNode(value) {}
