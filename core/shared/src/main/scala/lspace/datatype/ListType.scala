package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

object ListType extends DataTypeDef[ListType[Any]] {

  lazy val datatype = new ListType[List[Any]](None) {
    val iri: String = NS.types.`@list`
    labelMap ++= Map("en" -> NS.types.`@list`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(CollectionType.datatype)
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

  implicit def defaultListTypeCls[T, TOut, CTOut <: ClassType[_]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[ListType[List[T]], List[TOut], ListType[List[TOut]]] =
    new ClassTypeable[ListType[List[T]]] {
      type C  = List[TOut]
      type CT = ListType[List[TOut]]
      def ct: CT =
        if (clsTpbl.ct.iri.nonEmpty) ListType(clsTpbl.ct.asInstanceOf[ClassType[TOut]])
        else ListType.datatype.asInstanceOf[ListType[List[TOut]]]
    }

  def apply(): ListType[List[Any]] = datatype
  def apply[V](valueRange: ClassType[V]): ListType[List[V]] = {
    new ListType[List[V]](Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        List(NS.types.`@list`, valueRange.map(_.iri).filter(_.nonEmpty).map("(" + _ + ")").getOrElse(""))
          .filter(_.nonEmpty)
          .reduceLeft(_ + _)

      override lazy val _extendedClasses: List[_ <: DataType[_]] = datatype :: Nil
    }
  }
}

abstract class ListType[+T](val valueRange: Option[ClassType[Any]]) extends CollectionType[T] {

  override def `extends`(classType: ClassType[_]): Boolean =
    if (extendedClasses().contains(classType)) true
    else {
      classType match {
        case tpe: ListType[_] =>
          (valueRange, tpe.valueRange) match {
            case (Some(thisRange), Some(thatRange)) => thisRange.`@extends`(thatRange)
            case (None, Some(thatRange))            => false
            case (Some(thisRange), None)            => true
            case (None, None)                       => true
          }
        case _ => super.`extends`(classType)
      }
    }
}
