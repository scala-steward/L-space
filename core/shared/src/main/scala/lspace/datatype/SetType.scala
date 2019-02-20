package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

object SetType extends DataTypeDef[SetType[Any]] {

  lazy val datatype = new SetType[Any](Nil) {
    val iri: String                                             = NS.types.`@set`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@set`)
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

  implicit def defaultCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): ClassTypeable.Aux[SetType[T], List[TOut], SetType[TOut]] =
    new ClassTypeable[SetType[T]] {
      type C  = List[TOut]
      type CT = SetType[TOut]
      def ct: CT = //SetType(List(clsTpbl.ct)).asInstanceOf[SetType[TOut]]
        if (clsTpbl.ct.iri.nonEmpty) SetType(List(clsTpbl.ct)) else SetType.datatype.asInstanceOf[SetType[TOut]]
    }

  def apply[V: DefaultsToAny](valueRange: List[ClassType[V]]): SetType[V] = {
    if (valueRange.nonEmpty)
      new SetType[V](valueRange) {
        lazy val iri =
          List(NS.types.`@set`, "(", valueRange.map(_.iri).filter(_.nonEmpty).mkString("+"), ")")
            .filter(_.nonEmpty)
            .reduceLeft(_ + _)

        override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
      } else SetType.datatype.asInstanceOf[SetType[V]]
  }
}

abstract class SetType[+V](val valueRange: List[ClassType[V]]) extends CollectionType[List[V]]
