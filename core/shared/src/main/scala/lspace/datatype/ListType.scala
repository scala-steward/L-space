package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

object ListType extends DataTypeDef[ListType[Any]] {

  lazy val datatype = new ListType[Any](None) {
    val iri: String = NS.types.`@list`
    labelMap = Map("en" -> NS.types.`@list`)
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
        if (clsTpbl.ct.iri.nonEmpty) ListType(clsTpbl.ct) else ListType.datatype.asInstanceOf[ListType[TOut]]
    }

  def apply(): ListType[Any] = datatype
  def apply[V](valueRange: ClassType[V]): ListType[V] = {
    new ListType[V](Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        List(NS.types.`@list`, valueRange.map(_.iri).filter(_.nonEmpty).map("(" + _ + ")").getOrElse(""))
          .filter(_.nonEmpty)
          .reduceLeft(_ + _)

      override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
    }
  }
}

abstract class ListType[+V](val valueRange: Option[ClassType[V]]) extends CollectionType[List[V]]
