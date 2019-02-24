package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

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

//  def wrap(node: Node): ListType[Any] = {
//    ListType(
//      node
//        .out(CollectionType.keys.valueRangeClassType)
//        .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//        .flatten)
//  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): ListType[TOut] =
//    new ListType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[ListType[TOut]]

  implicit def defaultListTypeCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): ClassTypeable.Aux[ListType[T], List[TOut], ListType[TOut]] =
    new ClassTypeable[ListType[T]] {
      type C  = List[TOut]
      type CT = ListType[TOut]
      def ct: CT =
        if (clsTpbl.ct.iri.nonEmpty) ListType(List(clsTpbl.ct)) else ListType.datatype.asInstanceOf[ListType[TOut]]
    }

  def apply[V: DefaultsToAny](valueRange: List[ClassType[V]] = List()): ListType[V] = {
    if (valueRange.nonEmpty)
      new ListType[V](valueRange) {
        lazy val iri =
          List(NS.types.`@list`, "(", valueRange.map(_.iri).filter(_.nonEmpty).mkString("+"), ")")
            .filter(_.nonEmpty)
            .reduceLeft(_ + _)

        override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
      } else ListType.datatype.asInstanceOf[ListType[V]]
  }
}

abstract class ListType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]]
