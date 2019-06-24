package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

object SetType extends DataTypeDef[SetType[Any]] {

  lazy val datatype = new SetType[Set[Any]](None) {
    val iri: String = NS.types.`@set`
    labelMap = Map("en" -> NS.types.`@set`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

  //  def apply[V](valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.set}:[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[Set[V]](iri).getOrElse(new SetType(valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

//  def wrap(node: Node): SetType[Any] = {
//    SetType(
//      node
//        .out(CollectionType.keys.valueRange)
//        .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//        .flatten)
//  }

//  def apply[VT <: ClassType[_], TOut, CTOut <: ClassType[TOut]](valueRange: List[VT])(
//      implicit clsTpbl: ClassTypeable.Aux[VT, TOut, CTOut]): SetType[TOut] =
//    new SetType[TOut](valueRange.asInstanceOf[List[ClassType[TOut]]]).asInstanceOf[SetType[TOut]]

  implicit def defaultCls[T, TOut, CTOut <: ClassType[_]](implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[SetType[Set[T]], Set[TOut], SetType[Set[TOut]]] =
    new ClassTypeable[SetType[Set[T]]] {
      type C  = Set[TOut]
      type CT = SetType[Set[TOut]]
      def ct: CT = //SetType(List(clsTpbl.ct)).asInstanceOf[SetType[TOut]]
        if (clsTpbl.ct.iri.nonEmpty) SetType(clsTpbl.ct.asInstanceOf[ClassType[TOut]])
        else SetType.datatype.asInstanceOf[SetType[Set[TOut]]]
    }

  def apply(): SetType[Set[Any]] = datatype
  def apply[V](valueRange: ClassType[V]): SetType[Set[V]] = {
    new SetType[Set[V]](Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        List(NS.types.`@set`, valueRange.map(_.iri).filter(_.nonEmpty).map("(" + _ + ")").getOrElse(""))
          .filter(_.nonEmpty)
          .reduceLeft(_ + _)

      override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
    }
  }
}

abstract class SetType[+T](val valueRange: Option[ClassType[Any]]) extends CollectionType[T]
