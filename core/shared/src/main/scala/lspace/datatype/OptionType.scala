package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._
import lspace.util.types.DefaultsToAny

object OptionType extends DataTypeDef[OptionType[Any]] {

  lazy val datatype = new OptionType[Option[Any]](None) {
    val iri: String = NS.types.`@option`
    labelMap = Map("en" -> NS.types.`@option`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.datatype)
  }

  object keys extends CollectionType.Properties
  override lazy val properties: List[Property] = CollectionType.properties
  trait Properties extends CollectionType.Properties

  implicit def defaultListTypeCls[T, TOut, CTOut <: ClassType[TOut]](
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut])
    : ClassTypeable.Aux[OptionType[T], Option[TOut], OptionType[Option[TOut]]] =
    new ClassTypeable[OptionType[T]] {
      type C  = Option[TOut]
      type CT = OptionType[Option[TOut]]
      def ct: CT =
        if (clsTpbl.ct.iri.nonEmpty) OptionType(clsTpbl.ct)
        else OptionType.datatype.asInstanceOf[OptionType[Option[TOut]]]
    }

  def apply(): OptionType[Option[Any]] = datatype
  def apply[V](valueRange: ClassType[V]): OptionType[Option[V]] = {
    new OptionType[Option[V]](Some(valueRange).filter(_.iri.nonEmpty)) {
      lazy val iri =
        List(NS.types.`@option`, valueRange.map(_.iri).filter(_.nonEmpty).map("(" + _ + ")").getOrElse(""))
          .filter(_.nonEmpty)
          .reduceLeft(_ + _)

      override val _extendedClasses: () => List[_ <: DataType[_]] = () => datatype :: Nil
    }
  }
}

abstract class OptionType[+V](val valueRange: Option[ClassType[Any]]) extends CollectionType[V]
