package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

//import scala.collection.immutable.ListSet

object MapType extends DataTypeDef[MapType[Any, Any]] {

  lazy val datatype = new MapType[Any, Any](Nil, Nil) {
    val iri: String                                             = NS.types.`@map`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@map`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties {
    object keyRange
        extends PropertyDef(
          "@keyRange",
          "@keyRange",
          "A @keyRange",
          `@extends` = () => Property.default.`@range` :: Nil,
          `@range` = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
        )
    lazy val keyRangeClassType: TypedProperty[List[Node]] = keyRange + ListType(
      Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
//    lazy val keyRangeProperty: TypedProperty[Property]      = keyRange + DataType.default.`@property`
//    lazy val keyRangeDatatype: TypedProperty[DataType[Any]] = keyRange + DataType.default.`@datatype`
  }
  override lazy val properties: List[Property] = keys.keyRange :: CollectionType.properties
  trait Properties extends CollectionType.Properties {
    lazy val keyRange: Property                           = keys.keyRange
    lazy val keyRangeClassType: TypedProperty[List[Node]] = keys.keyRangeClassType
//    lazy val keyRangeProperty: TypedProperty[Property]      = keys.keyRangeProperty
//    lazy val keyRangeDatatype: TypedProperty[DataType[Any]] = keys.keyRangeDatatype
  }

  //  def apply[K](keyType: ClassType[K])(implicit graph: Graph)= {
  //    val iri = s"${ldcontext.types.map}:[${keyType.iri}]"
  //    new MapType[K, Any](keyType, valueType, graph.getDataType(iri).getOrElse(graph.nodes.upsert(iri)))
  //  }
  //  def apply[K, V](keyRange: List[ClassType[K]], valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.map}:[${keyRange.map(_.iri).toList.sorted}],[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[Map[K, V]](iri).getOrElse(new MapType(keyRange, valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(keys.keyRange, keyRange.map(graph.nodeURLType -> _))
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

//  def wrap(node: Node): MapType[Any, Any] = {
//    MapType(
//      node.out(keys.keyRange).collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }.flatten,
//      node
//        .out(CollectionType.keys.valueRange)
//        .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//        .flatten
//    )
//  }

//  def apply[KT <: ClassType[_],
//            KTOut,
//            CKTOut <: ClassType[KTOut],
//            VT <: ClassType[_],
//            VTOut,
//            CVTOut <: ClassType[VTOut]](keyRange: List[KT], valueRange: List[VT])(
//      implicit kclsTpbl: ClassTypeable.Aux[KT, KTOut, CKTOut],
//      vclsTpbl: ClassTypeable.Aux[VT, VTOut, CVTOut]): MapType[KTOut, VTOut] =
//    new MapType(keyRange.asInstanceOf[List[ClassType[KTOut]]], valueRange.asInstanceOf[List[ClassType[VTOut]]])
//      .asInstanceOf[MapType[KTOut, VTOut]]

  implicit def defaultCls[
      K,
      KT[+Z] <: ClassType[Z],
      V,
      VT[+Z] <: ClassType[Z],
      KOut,
      KTOut[+Z] <: ClassType[Z],
      VOut,
      VTOut[+Z] <: ClassType[Z]
  ](implicit clsTpblK: ClassTypeable.Aux[KT[K], KOut, KTOut[KOut]],
    clsTpblV: ClassTypeable.Aux[VT[V], VOut, VTOut[VOut]])
    : ClassTypeable.Aux[MapType[K, V], Map[KOut, VOut], MapType[KOut, VOut]] =
    new ClassTypeable[MapType[K, V]] {
      type C  = Map[KOut, VOut]
      type CT = MapType[KOut, VOut]
      def ct: CT = //MapType(List(clsTpblK.ct), List(clsTpblV.ct))
        if (clsTpblK.ct.iri.nonEmpty || clsTpblV.ct.iri.nonEmpty) MapType(List(clsTpblK.ct), List(clsTpblV.ct))
        else MapType.datatype.asInstanceOf[MapType[KOut, VOut]]
    }

  def apply[K: DefaultsToAny, V: DefaultsToAny](keyRange: List[ClassType[K]],
                                                valueRange: List[ClassType[V]]): MapType[K, V] = {
    if (keyRange.nonEmpty || valueRange.nonEmpty)
      new MapType[K, V](keyRange, valueRange) {
        lazy val iri =
          //        if (keyRange.filter(_.iri.nonEmpty).isEmpty && valueRange.filter(_.iri.nonEmpty).isEmpty) NS.types.`@map`
          //        else
          s"${NS.types.`@map`}(${keyRange.map(_.iri).filter(_.nonEmpty).mkString("+")})(${valueRange.map(_.iri).filter(_.nonEmpty).mkString("+")})"

        override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
      } else MapType.datatype.asInstanceOf[MapType[K, V]]
  }
}

abstract class MapType[K, V](val keyRange: List[ClassType[K]], val valueRange: List[ClassType[V]])
    extends CollectionType[Map[K, V]]
