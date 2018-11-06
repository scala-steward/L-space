package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

import scala.collection.immutable.ListSet

object ListType {

  def wrap(node: Node): ListType[Any] = {
    ListType(
      node
        .out(CollectionType.keys.valueRange)
        .collect { case node: Node => node }
        .map(node.graph.ns.getClassType))
  }

  def apply[V, VT[+Z] <: ClassType[Z]](valueRange: List[VT[V]]) = new ListType(valueRange)

  implicit def defaultListTypeCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): ClassTypeable.Aux[ListType[T], List[TOut], ListType[TOut]] =
    new ClassTypeable[ListType[T]] {
      type C  = List[TOut]
      type CT = ListType[TOut]
      def ct: CT = ListType(List(clsTpbl.ct))
    }
}

class ListType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]] {
  lazy val iri = s"${NS.types.list}/${valueRange.map(_.iri).sorted.mkString("+")}"
  //  override lazy val extendedClasses: List[DataType[Iterable[V]]] = List(CollectionType[V](valueRange))

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.default[V])
}
