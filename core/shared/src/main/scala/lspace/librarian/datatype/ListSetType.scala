package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object ListSetType {

  //  def apply[V](valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.listset}:[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[ListSet[V]](iri).getOrElse(new ListSetType(valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.toList.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

  def wrap(node: Node): ListSetType[Any] = {
    ListSetType(
      node
        .out(CollectionType.keys.valueRange)
        .collect { case node: Node => node }
        .map(node.graph.ns.getClassType)
    ).asInstanceOf[ListSetType[Any]]
  }

  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): ListSetType[TOut] =
    new ListSetType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[ListSetType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[TOut]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[ListSetType[T], List[TOut], ListSetType[TOut]] =
    new ClassTypeable[ListSetType[T]] {
      type C  = List[TOut]
      type CT = ListSetType[TOut]
      def ct: CT = new ListSetType(List(clsTpbl.ct)).asInstanceOf[ListSetType[TOut]]
    }
}

class ListSetType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]] {

  val iri: String = s"${NS.types.`@listset`}/${valueRange.map(_.iri).sorted.mkString("+")}"
  //  override lazy val extendedClasses: List[DataType[Iterable[V]]] = List(CollectionType[V](valueRange))

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.default[V])
}
