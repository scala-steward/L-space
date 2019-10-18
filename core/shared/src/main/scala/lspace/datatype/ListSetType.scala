package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

import scala.collection.immutable.ListSet

object ListSetType extends DataTypeDef[ListSetType[Any]] {

  lazy val datatype = new ListSetType[ListSet[Any]](None) {
    val iri: String = NS.types.`@listset`
    labelMap ++= Map("en" -> NS.types.`@listset`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(CollectionType.datatype)
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

//  def wrap(node: Node): ListSetType[Any] = {
//    ListSetType(
//      node
//        .out(CollectionType.keys.valueRange)
//        .collect { case node: Node => node }
//        .map(node.graph.ns.classtypes.get)
//    ).asInstanceOf[ListSetType[Any]]
//  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): ListSetType[TOut] =
//    new ListSetType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[ListSetType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[_]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[ListSetType[ListSet[T]], ListSet[TOut], ListSetType[ListSet[TOut]]] =
    new ClassTypeable[ListSetType[ListSet[T]]] {
      type C  = ListSet[TOut]
      type CT = ListSetType[ListSet[TOut]]
      def ct: CT = //ListSetType(List(clsTpbl.ct)).asInstanceOf[ListSetType[TOut]]
        if (clsTpbl.ct.iri.nonEmpty)
          ListSetType(clsTpbl.ct.asInstanceOf[ClassType[TOut]])
        else ListSetType.datatype.asInstanceOf[ListSetType[ListSet[TOut]]]
    }

  def apply(): ListSetType[ListSet[Any]] = datatype
  def apply[V](valueRange: ClassType[V]): ListSetType[ListSet[V]] = {
    new ListSetType[ListSet[V]](Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        List(NS.types.`@listset`, "(", valueRange.map(_.iri).filter(_.nonEmpty).getOrElse(""), ")")
          .filter(_.nonEmpty)
          .reduceLeft(_ + _)
      override lazy val _extendedClasses: List[_ <: DataType[_]] = datatype :: Nil
    }
  }
}

abstract class ListSetType[+V](val valueRange: Option[ClassType[Any]]) extends CollectionType[V] {
  override def `extends`(classType: ClassType[_]): Boolean =
    if (extendedClasses().contains(classType)) true
    else {
      classType match {
        case tpe: ListSetType[_] =>
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
