package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object ListSetType extends DataTypeDef[ListSetType[Any]] {

  lazy val datatype = new ListSetType[Any](Nil) {
    val iri: String                                             = NS.types.`@listset`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@listset`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

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
        .map(node.graph.ns.classtypes.get)
    ).asInstanceOf[ListSetType[Any]]
  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): ListSetType[TOut] =
//    new ListSetType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[ListSetType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[TOut]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[ListSetType[T], List[TOut], ListSetType[TOut]] =
    new ClassTypeable[ListSetType[T]] {
      type C  = List[TOut]
      type CT = ListSetType[TOut]
      def ct: CT = ListSetType(List(clsTpbl.ct)).asInstanceOf[ListSetType[TOut]]
    }

  def apply[V](valueRange: List[ClassType[V]]): ListSetType[V] = new ListSetType[V](valueRange) {
    lazy val iri =
      List(NS.types.`@listset`, "(", valueRange.map(_.iri).filter(_.nonEmpty).sorted.mkString("+"), ")")
        .filter(_.nonEmpty)
        .mkString("/")
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
  }
}

abstract class ListSetType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]]
