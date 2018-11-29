package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types._

object As extends StepDef("As") with StepWrapper[As[Any, String]] {

  /*  import shapeless._
  import shapeless.ops.hlist._
  import syntax.singleton._

  class NamedLabel[S <: String](val name: S)
  object NamedLabel {
    implicit def _fromString(name: String)                    = new NamedLabel(name.narrow)
    implicit def _toString(namedLabel: NamedLabel[_]): String = namedLabel.name.toString
//    def apply(name: String)               = new NamedLabel(name.narrow)
  }*/

  def wrap(node: Node): As[Any, String] = node match {
    case node: As[Any, String] => node
    case _                     => As[Any, String](node.out(As.keys.nameString).head, node)
  }

  object keys {
    private val nameNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/As/name")
    nameNode.addLabel(Property.ontology)
    nameNode --- Property.default.`@label` --> "traversal" --- Property.default.`@language` --> "en"
    nameNode --- Property.default.`@comment` --> "A traversal which must have a non-empty result" --- Property.default.`@language` --> "en"
    nameNode --- Property.default.`@range` --> DataType.default.`@string`

    lazy val name: Property               = Property(nameNode)
    val nameString: TypedProperty[String] = name + DataType.default.`@string`
  }

  def apply[T, Tname <: String](label: Tname): As[T, Tname] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.nameString, label.toString)
    As[T, Tname](label, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.name
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class As[T, name <: String] private (label: name, override val value: Node)
    extends WrappedNode(value)
    with TraverseStep {
  def _maphelper = List[T]().head

  override def prettyPrint: String = "as(" + label + ")"
}
