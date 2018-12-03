package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

import scala.collection.immutable.ListSet

object VectorType {

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
        .collect { case node: Node => node }
        .map(node.graph.ns.getClassType))
  }

  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): VectorType[TOut] =
    new VectorType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[VectorType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[TOut]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[VectorType[T], Vector[TOut], VectorType[TOut]] =
    new ClassTypeable[VectorType[T]] {
      type C  = Vector[TOut]
      type CT = VectorType[TOut]
      def ct: CT = new VectorType(List(clsTpbl.ct))
    }
}

class VectorType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[Vector[V]] {
  lazy val iri = s"${NS.types.`@list`}/${valueRange.map(_.iri).sorted.mkString("+")}"
  //  override lazy val extendedClasses: List[DataType[Iterable[V]]] = List(CollectionType[V](valueRange))
  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.default[V])
}
