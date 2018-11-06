package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

import scala.collection.immutable.ListSet

object SetType {

  //  def apply[V](valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.set}:[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[Set[V]](iri).getOrElse(new SetType(valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

  def wrap(node: Node): SetType[Any] = {
    SetType(
      node
        .out(CollectionType.keys.valueRange)
        .collect { case node: Node => node }
        .map(node.graph.ns.getClassType))
  }

  def apply[V, VT[+Z] <: ClassType[Z]](valueRange: List[VT[V]]) = new SetType(valueRange)

  implicit def defaultCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): ClassTypeable.Aux[SetType[T], List[TOut], SetType[TOut]] =
    new ClassTypeable[SetType[T]] {
      type C  = List[TOut]
      type CT = SetType[TOut]
      def ct: CT = SetType(List(clsTpbl.ct)).asInstanceOf[SetType[TOut]]
    }
}

class SetType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]] {

  val iri = s"${NS.types.set}/${valueRange.map(_.iri).sorted.mkString("+")}"
  //  override lazy val extendedClasses: List[DataType[Iterable[V]]] = List(CollectionType[V](valueRange))
  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.default[V])
}
