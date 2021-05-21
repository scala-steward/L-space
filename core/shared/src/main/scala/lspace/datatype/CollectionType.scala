package lspace.datatype

import lspace.NS
import lspace.NS.types
import lspace.structure.util.ClassTypeable
import lspace.structure._

import scala.annotation.tailrec

//TODO: a note on collections: auto-merging resources by their iri can result in obsolete references from within collection structures.
//TODO: create a RefNode, RefEdge and RefValue to mitigate?
object CollectionType extends DataTypeDef[CollectionType[Iterable[Any]]] {

  val datatype = new CollectionType[Iterable[Any]] {
    val iri: String = NS.types.`@collection`
    labelMap ++= Map("en" -> NS.types.`@collection`)
    override lazy val _extendedClasses: List[_ <: DataType[_]] = List(StructuredType.datatype)
  }

  object keys {
    object valueRange
        extends PropertyDef(
          "@valueRange",
          "@valueRange",
          "A @valueRange is a strict lowerbound of classtypes, this ensures type-safe casting of collections",
          `@extends` = Property.default.`@range` :: Nil,
          `@range` = ListType(NodeURLType.datatype) :: Nil
        )
    lazy val valueRangeClassType: TypedProperty[List[Node]] = valueRange as ListType(NodeURLType.datatype)
  }
  override lazy val properties: List[Property] = keys.valueRange :: Nil //StructuredValue.properties
  trait Properties extends StructuredType.Properties {
    lazy val valueRange: Property                           = keys.valueRange
    lazy val valueRangeClassType: TypedProperty[List[Node]] = keys.valueRangeClassType
  }

  def apply[V](valueRange: List[ClassType[V]]) = new CollectionType[Iterable[V]] {
    val iri: String = s"${NS.types.`@collection`}/${valueRange.map(_.iri).sorted.mkString("+")}"
  }

  implicit def clsCollection[T]: ClassTypeable.Aux[CollectionType[T], Iterable[Any], CollectionType[Iterable[Any]]] =
    new ClassTypeable[CollectionType[T]] {
      type C  = Iterable[Any]
      type CT = CollectionType[Iterable[Any]]
      def ct: CT = datatype
    }

  private val separators = Set('(', ')', '+')

  private def getTypes(iri: String): (Option[ClassType[Any]], String) = {
    iri.splitAt(iri.indexWhere(separators.contains)) match {
      case ("", iri) if iri.startsWith(")") => None                                    -> iri.drop(1)
      case ("", iri)                        => Some(ClassType.classtypes.get(iri).get) -> ""
      case (iri, tail) if tail.startsWith("(") =>
        iri match {
          case types.`@list` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) Some(ListType(valueTypes.get)) else Some(ListType.datatype)) -> newTail
          case types.`@listset` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) Some(ListSetType(valueTypes.get)) else Some(ListSetType.datatype)) -> newTail
          case types.`@set` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) Some(SetType(valueTypes.get)) else Some(SetType.datatype)) -> newTail
          case types.`@vector` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) Some(VectorType(valueTypes.get)) else Some(VectorType.datatype)) -> newTail
          case types.`@map` =>
            val (keyTypes, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("map without second block")
            val (valueTypes, newTail2) = getTypes(newTail.drop(1))
            (if (keyTypes.nonEmpty || valueTypes.nonEmpty)
               Some(MapType(keyTypes.getOrElse(ClassType.stubAny), valueTypes.getOrElse(ClassType.stubAny)))
             else Some(MapType.datatype)) -> newTail2
          case types.`@tuple` =>
            @tailrec
            def getT(tail: String, types: List[Option[ClassType[Any]]]): (List[Option[ClassType[Any]]], String) = {
              val (valueTypes, newTail) = getTypes(tail.drop(1))
              if (newTail.startsWith("("))
                getT(newTail, types :+ (if (valueTypes.nonEmpty) Some(valueTypes.get) else None))
              else
                (types :+ (if (valueTypes.nonEmpty) Some(valueTypes.get) else None)) -> newTail
            }
            val (rangeTypes, newTail) = getT(tail, List())
            Some(TupleType(rangeTypes)) -> newTail
          case _ =>
            scribe.error("cannot parse : " + iri)
            throw new Exception("cannot parse : " + iri)
        }
      case (iri, tail) if tail.startsWith(")") => get(iri) -> tail.dropWhile(_ == ')')
      case (iri, tail) if tail.startsWith("+") =>
        val (tailTypes, newTail) = getTypes(tail.drop(1))
        (get(iri).toList ++ tailTypes).reduceOption(_ + _) -> newTail
      case _ => throw new Exception(s"invalid collection type $iri")
    }
  }

  def get(iri: String): Option[DataType[Any]] = //TODO: .get (Task) instead of .cached
    {
      ClassType.classtypes
        .get(iri)
        .orElse(getTypes(iri) match {
          case (Some(ct), "") =>
            Some(ct)
          case (Some(ct), tail) =>
            scribe.warn(s"got type but tail is not empty, residu is: $tail")
            Some(ct)
          case (None, tail) =>
            scribe.warn(s"no classtype construct build for $iri, residu is: $tail")
            None
        })
        .asInstanceOf[Option[DataType[Any]]]
    }
}

trait CollectionType[+T] extends StructuredType[T] {
  override lazy val _extendedClasses: List[_ <: DataType[_]] = List(StructuredType.datatype)
  override lazy val _properties: List[Property]              = List(CollectionType.keys.valueRange)
}
