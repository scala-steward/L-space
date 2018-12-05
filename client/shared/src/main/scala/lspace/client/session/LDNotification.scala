package lspace.client.session

import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure.Node

abstract class LDNotification(node: Node) extends WrappedNode(node)

object LDNotification {

  //  object Traverse {
  //    val uri = lspace.NS.vocab.Lspace + "action/traverse"
  //    val ontology = Ontology(uri)
  //
  //    val traversal = LinkPropertyKey("traversal", s"$uri/traversal")(NodeType(Traversal.ontology))
  //    traversal.label += "en" -> "Traversal"
  //    traversal.comment += "en" -> "A complete traversal, including the graph target to traverse on"
  //    ontology.addProperty(traversal)
  //
  //    ontology.status.update(CacheStatus.CACHED)
  //  }
  //  case class Traverse(iri: String = "") extends NodeResource with LDNotification {
  //    def traversal: Traversal[_, _, _, _ <: HList] =
  //      this.out(Traverse.traversal).map(_.asInstanceOf[Traversal[_, _, _, _ <: HList]]).head
  //  }

  case class Get(uri: String, node: Node) extends LDNotification(node)

  /**
    *
    * @param email provisioned by
    */
  case class SessionProvisioned(email: String, node: Node) extends LDNotification(node)

  /**
    *
    * @param message reason
    */
  case class SessionRevoked(message: String, node: Node) extends LDNotification(node)
}
