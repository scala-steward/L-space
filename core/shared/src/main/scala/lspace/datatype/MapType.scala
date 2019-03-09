package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

//import scala.collection.immutable.ListSet

object MapType extends DataTypeDef[MapType[Any, Any]] {

  lazy val datatype = new MapType[Any, Any](Nil, Nil) {
    val iri: String = NS.types.`@map`
    labelMap = Map("en" -> NS.types.`@map`)
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

  def apply[K: DefaultsToAny, V: DefaultsToAny](keyRange: List[ClassType[K]] = List(),
                                                valueRange: List[ClassType[V]] = List()): MapType[K, V] = {
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
