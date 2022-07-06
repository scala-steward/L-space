package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._

object MapType extends DataTypeDef[MapType[Map[Any, Any]]] {

  lazy val datatype = new MapType[Map[Any, Any]](None, None) {
    val iri: String = NS.types.`@map`
    labelMap ++= Map("en" -> NS.types.`@map`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties {
    object keyRange
        extends PropertyDef(
          "@keyRange",
          "@keyRange",
          "A @keyRange",
          `@extends` = Property.default.`@range` :: Nil,
          `@range` = ListType() :: Nil
        )
    lazy val keyRangeClassType: TypedProperty[List[Node]] = keyRange.as(ListType(NodeURLType.datatype))
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
  ](implicit
    clsTpblK: ClassTypeable.Aux[KT[K], KOut, KTOut[KOut]],
    clsTpblV: ClassTypeable.Aux[VT[V], VOut, VTOut[VOut]]
  ): ClassTypeable.Aux[MapType[Map[K, V]], Map[KOut, VOut], MapType[Map[KOut, VOut]]] =
    new ClassTypeable[MapType[Map[K, V]]] {
      type C  = Map[KOut, VOut]
      type CT = MapType[Map[KOut, VOut]]
      def ct: CT = // MapType(List(clsTpblK.ct), List(clsTpblV.ct))
        if (clsTpblK.ct.iri.nonEmpty || clsTpblV.ct.iri.nonEmpty) MapType(clsTpblK.ct, clsTpblV.ct)
        else MapType.datatype.asInstanceOf[MapType[Map[KOut, VOut]]]
    }

  def apply(): MapType[Map[Any, Any]] = datatype
  def apply[K, V](keyRange: ClassType[K], valueRange: ClassType[V]): MapType[Map[K, V]] =
    new MapType[Map[K, V]](Some(keyRange).filter(_.iri.nonEmpty), Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        //        if (keyRange.filter(_.iri.nonEmpty).isEmpty && valueRange.filter(_.iri.nonEmpty).isEmpty) NS.types.`@map`
        //        else
        s"${NS.types.`@map`}(${keyRange.map(_.iri).filter(_.nonEmpty).getOrElse("")})(${valueRange.map(_.iri).filter(_.nonEmpty).getOrElse("")})"

      override protected def _extendedClasses: List[ClassType[Any]] = List(datatype)
    }
}

abstract class MapType[+T](val keyRange: Option[ClassType[Any]], val valueRange: Option[ClassType[Any]])
    extends CollectionType[T] {
  override def `extends`(classType: ClassType[_]): Boolean =
    if (iri == classType.iri) false
    else if (this.extendedClasses().contains(classType)) true
    else {
      classType match {
        case tpe: MapType[_] =>
          ((keyRange, tpe.keyRange) match {
            case (Some(thisRange), Some(thatRange)) => thisRange.iri == thatRange.iri || thisRange.`@extends`(thatRange)
            case (None, Some(_))                    => false
            case (Some(_), None)                    => true
            case (None, None)                       => true
          }) && ((valueRange, tpe.valueRange) match {
            case (Some(thisRange), Some(thatRange)) => thisRange.iri == thatRange.iri || thisRange.`@extends`(thatRange)
            case (None, Some(_))                    => false
            case (Some(_), None)                    => true
            case (None, None)                       => true
          })
        case _ => super.`extends`(classType)
      }
    }
}
