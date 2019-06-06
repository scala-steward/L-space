package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

object OptionType extends DataTypeDef[OptionType[Any]] {

  lazy val datatype = new OptionType[Any](None) {
    val iri: String = NS.types.`@option`
    labelMap = Map("en" -> NS.types.`@option`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

  implicit def defaultListTypeCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[OptionType[T], Option[TOut], OptionType[TOut]] =
    new ClassTypeable[OptionType[T]] {
      type C  = Option[TOut]
      type CT = OptionType[TOut]
      def ct: CT =
        if (clsTpbl.ct.iri.nonEmpty) OptionType(Some(clsTpbl.ct))
        else OptionType.datatype.asInstanceOf[OptionType[TOut]]
    }

  def apply[V: DefaultsToAny](valueRange: Option[ClassType[V]] = None): OptionType[V] = {
    if (valueRange.nonEmpty)
      new OptionType[V](valueRange) {
        lazy val iri =
          List(NS.types.`@option`, "(", valueRange.map(_.iri).filter(_.nonEmpty).getOrElse(""), ")")
            .filter(_.nonEmpty)
            .reduceLeft(_ + _)

        override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
      } else OptionType.datatype.asInstanceOf[OptionType[V]]
  }
}

abstract class OptionType[+V](val valueRange: Option[ClassType[V]]) extends CollectionType[Option[V]]
