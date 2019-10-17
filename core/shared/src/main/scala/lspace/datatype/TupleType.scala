package lspace.datatype

import lspace.NS
import lspace.NS.types
import lspace.structure.util.ClassTypeable
import lspace.structure._

//TODO: type construction without nested types should default to @tuple, @tuple2, @tuple3 or @tuple4 (example at @list)
object TupleType extends DataTypeDef[TupleType[_]] {

  def apply[T](rangeTypes: List[Option[ClassType[Any]]] = List()): TupleType[T] =
    if (rangeTypes.isEmpty) make[T]
    else
      new TupleType[T](rangeTypes.map(_.filter(_.iri.nonEmpty)))

  lazy val datatype = new TupleType[Any] {
    override lazy val iri: String = NS.types.`@tuple`
    labelMap ++= Map("en" -> NS.types.`@tuple`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(StructuredType.datatype)
  }

  def make[T] = new TupleType[T] {
    override lazy val iri: String = NS.types.`@tuple`
    labelMap ++= Map("en" -> NS.types.`@tuple`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(StructuredType.datatype)
  }

  object keys extends StructuredType.Properties { //TODO: change to PropertyDef
    object range
        extends PropertyDef(
          "@tuplerange",
          label = "@tuplerange",
          comment = "@tuplerange",
          `@extends` = Property.default.`@range` :: Nil,
          `@range` = ListType(OptionType(NodeURLType.datatype)) :: Nil
        )
    lazy val _rangeClassType: TypedProperty[List[Option[Node]]] = range as ListType(OptionType(NodeURLType.datatype))

  }
  override lazy val properties: List[Property] = keys.range.property :: StructuredType.properties
  trait Properties extends StructuredType.Properties {}
}

class TupleType[+T](val rangeTypes: List[Option[ClassType[Any]]] = List()) extends StructuredType[T] {
  lazy val iri = {
    //    if (_1stRange.filter(_.iri.nonEmpty).isEmpty && _2ndRange.filter(_.iri.nonEmpty).isEmpty) NS.types.`@tuple` + "2"
    //    else
    val iriTail = "(" + rangeTypes
      .foldLeft(List[String]()) {
        case (tail, rangeTypes) =>
          tail :+ rangeTypes.map(_.iri).filter(_.nonEmpty).getOrElse("")
//            .foldLeft(List[String]()) {
//              case (tail, rangeType) => tail :+ rangeType.iri
//            }
//            .getOrElse("")
      }
      .mkString(")(") + ")"
    s"${types.`@tuple`}$iriTail"
  }

  override lazy val _extendedClasses: List[_ <: DataType[_]] = List(TupleType.datatype)
}
