package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object VectorType extends DataTypeDef[VectorType[Any]] {

  lazy val datatype = new VectorType[Any](Nil) {
    val iri: String                                             = NS.types.`@vector`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@vector`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

  //  def apply[V](valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.list}:[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[Vector[V]](iri).getOrElse(new VectorType[V](valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

  def wrap(node: Node): VectorType[Any] = {
    VectorType(
      node
        .out(CollectionType.keys.valueRange)
        .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
        .flatten)
  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): VectorType[TOut] =
//    new VectorType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[VectorType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[TOut]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[VectorType[T], Vector[TOut], VectorType[TOut]] =
    new ClassTypeable[VectorType[T]] {
      type C  = Vector[TOut]
      type CT = VectorType[TOut]
      def ct: CT = VectorType(List(clsTpbl.ct))
    }

  def apply[V](valueRange: List[ClassType[V]]): VectorType[V] = new VectorType[V](valueRange) {
    lazy val iri =
      List(NS.types.`@vector`, "(", valueRange.map(_.iri).filter(_.nonEmpty).sorted.mkString("+"), ")")
        .filter(_.nonEmpty)
        .reduceLeft(_ + _)

    override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
  }
}

abstract class VectorType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[Vector[V]]
