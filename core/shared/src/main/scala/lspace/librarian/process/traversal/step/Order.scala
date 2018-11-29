package lspace.librarian.process.traversal.step

import lspace.librarian.datatype.TextType
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.HList
import lspace.types._

object Order extends StepDef("Order") with StepWrapper[Order] {

  object Orderable {
    implicit def IsNumeric[T[+Z] <: NumericType[Z]]: Orderable[T]   = new Orderable[T] {}
    implicit def IsString[T[+Z] <: TextType[Z]]: Orderable[T]       = new Orderable[T] {}
    implicit def IsTemporal[T[+Z] <: CalendarType[Z]]: Orderable[T] = new Orderable[T] {}
    implicit def IsQuantity[T[+Z] <: QuantityType[Z]]: Orderable[T] = new Orderable[T] {}
    implicit def IsColor[T[+Z] <: ColorType[Z]]: Orderable[T]       = new Orderable[T] {}
  }
  sealed trait Orderable[T[+Z]]

  def wrap(node: Node): Order = node match {
    case node: Order => node
    case _ =>
      Order(
        node
          .out(Order.keys.byTraversal)
          .map(Traversal.wrap(_)(DetachedGraph)(DataType.default.default))
          .head,
        node.out(Order.keys.increasingBoolean).take(1).headOption.getOrElse(true),
        node
      )
  }

  object keys {
    private val byNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Order/by")
    byNode.addLabel(Property.ontology)
    byNode --- Property.default.`@label` --> "by" --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@comment` --> "A traversal .." --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@range` --> Traversal.ontology
    lazy val by                          = Property(byNode)
    val byTraversal: TypedProperty[Node] = by + Traversal.ontology

    private val increasingNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Order/increasing")
    increasingNode.addLabel(Property.ontology)
    increasingNode --- Property.default.`@label` --> "increasing" --- Property.default.`@language` --> "en"
    increasingNode --- Property.default.`@comment` --> "Set to true to sort ascending" --- Property.default.`@language` --> "en"
    increasingNode --- Property.default.`@range` --> DataType.default.`@boolean`
    lazy val increasing: Property                 = Property(increasingNode)
    val increasingBoolean: TypedProperty[Boolean] = increasing + DataType.default.`@boolean`
  }

  def apply(by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList], increasing: Boolean = true): Order = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.byTraversal, by.self)
    if (!increasing) node.addOut(keys.increasingBoolean, increasing)
    Order(by, increasing, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.by
  ontologyNode --- Property.default.`@properties` --> keys.increasing
  //  MemGraphDefault.ns.storeOntology(ontology)
}

//case class Order(key: PropertyKey, increasing: Boolean = true) extends TraverseStep /*with ModulateStep[ZeroOrMoreBy]*/ {
case class Order private (by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList],
                          increasing: Boolean,
                          override val value: Node)
    extends WrappedNode(value)
    with CollectingBarrierStep /*with ModulateStep[ZeroOrMoreBy]*/ {
  override def prettyPrint: String = "order(" + by.toString + ")"
}
