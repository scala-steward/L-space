package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

import scala.collection.immutable.ListSet

object ListType extends DataTypeDef[ListType[Any]] {

  lazy val datatype = new ListType[Any](Nil) {
    val iri: String                                             = NS.types.`@list`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@list`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

  def wrap(node: Node): ListType[Any] = {
    ListType(
      node
        .out(CollectionType.keys.valueRange)
        .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
        .flatten)
  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): ListType[TOut] =
//    new ListType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[ListType[TOut]]

  implicit def defaultListTypeCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): ClassTypeable.Aux[ListType[T], List[TOut], ListType[TOut]] =
    new ClassTypeable[ListType[T]] {
      type C  = List[TOut]
      type CT = ListType[TOut]
      def ct: CT = ListType(List(clsTpbl.ct))
    }

  def apply[V](valueRange: List[ClassType[V]]): ListType[V] = new ListType[V](valueRange) {
    lazy val iri =
      List(NS.types.`@list`, "(", valueRange.map(_.iri).filter(_.nonEmpty).sorted.mkString("+"), ")")
        .filter(_.nonEmpty)
        .mkString("/")
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
  }
}

abstract class ListType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]]
