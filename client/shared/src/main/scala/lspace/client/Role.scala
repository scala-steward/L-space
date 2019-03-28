package lspace.client

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure.OntologyDef
import lspace.structure._
import lspace.structure.Property.default._
import monix.eval.Task

object Role extends OntologyDef(lspace.NS.vocab.Lspace + "Role", Set(), "Role", "A role ..") {

  //TODO: test implicit graph helper-functions
  implicit class WithGraph(graph: Graph) {
    def newRole(iri: String): Task[Role] = {
      for {
        node <- graph.nodes.create(ontology)
        _    <- node.addOut(typed.iriUrlString, iri)
      } yield new Role(node)
    }
  }
  def apply(iri: String): Task[Role] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(typed.iriUrlString, iri)
    } yield new Role(node)
  }
  def wrap(node: Node): Role = node match {
    case node: Role => node
    case _          => Role(node)
  }

  object keys {
    object `schema:name`
        extends PropertyDef(lspace.NS.vocab.schema + "name",
                            label = "name",
                            `@range` = () => DataType.default.`@string` :: Nil)
    val `schema:name@String` = `schema:name` + DataType.default.`@string`
  }
  override lazy val properties: List[Property] = keys.`schema:name` :: Nil
  trait Properties {
    val `schema:name`: Property = keys.`schema:name`
    val `schema:name@String`    = keys.`schema:name@String`
  }
}

case class Role private (override val value: Node) extends WrappedNode(value) {}
