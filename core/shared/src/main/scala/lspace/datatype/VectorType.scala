package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._

object VectorType extends DataTypeDef[VectorType[Any]] {

  lazy val datatype = new VectorType[Vector[Any]](None) {
    val iri: String = NS.types.`@vector`
    labelMap ++= Map("en" -> NS.types.`@vector`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

  //  def apply[V](valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.list}:[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[Vector[V]](iri).getOrElse(new VectorType[V](valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

//  def wrap(node: Node): VectorType[Any] = {
//    VectorType(
//      node
//        .out(CollectionType.keys.valueRange)
//        .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//        .flatten)
//  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): VectorType[TOut] =
//    new VectorType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[VectorType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[_]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[VectorType[Vector[T]], Vector[TOut], VectorType[Vector[TOut]]] =
    new ClassTypeable[VectorType[Vector[T]]] {
      type C  = Vector[TOut]
      type CT = VectorType[Vector[TOut]]
      def ct: CT =
        if (clsTpbl.ct.iri.nonEmpty) VectorType(clsTpbl.ct.asInstanceOf[ClassType[TOut]])
        else VectorType.datatype.asInstanceOf[VectorType[Vector[TOut]]]
    }

  def apply(): VectorType[Vector[Any]] = datatype
  def apply[V](valueRange: ClassType[V]): VectorType[Vector[V]] = {
    new VectorType[Vector[V]](Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        List(NS.types.`@vector`, valueRange.map(_.iri).filter(_.nonEmpty).map("(" + _ + ")").getOrElse(""))
          .filter(_.nonEmpty)
          .reduceLeft(_ + _)

      override lazy val _extendedClasses: List[_ <: DataType[_]] = datatype :: Nil
    }
  }
}

abstract class VectorType[+V](val valueRange: Option[ClassType[Any]]) extends CollectionType[V] {
  override def `extends`(classType: ClassType[_]): Boolean =
    if (extendedClasses().contains(classType)) true
    else {
      classType match {
        case tpe: VectorType[_] =>
          (valueRange, tpe.valueRange) match {
            case (Some(thisRange), Some(thatRange)) => thisRange.`@extends`(thatRange)
            case (None, Some(_))            => false
            case (Some(_), None)            => true
            case (None, None)                       => true
          }
        case _ => super.`extends`(classType)
      }
    }
}
