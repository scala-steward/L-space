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
    val iri: String                                             = NS.types.`@collection`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@collection`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  }

  object keys {
    object valueRange
        extends PropertyDef(
          "@valueRange",
          "@valueRange",
          "A @valueRange is a strict lowerbound of classtypes, this ensures type-safe casting of collections",
          `@extends` = () => Property.default.`@range` :: Nil,
          `@range` = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
        )
    lazy val valueRangeClassType: TypedProperty[List[Node]] = valueRange + ListType(
      Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
  }
  override lazy val properties: List[Property] = keys.valueRange :: Nil //StructuredValue.properties
  trait Properties { //extends StructuredValue.Properties {
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

  private def getTypes(iri: String): (List[ClassType[Any]], String) = {
    iri.splitAt(iri.indexWhere(separators.contains)) match {
      case ("", iri) if iri.startsWith(")") => List()                                     -> iri.drop(1)
      case ("", iri)                        => List(ClassType.classtypes.cached(iri).get) -> ""
      case (iri, tail) if tail.startsWith("(") =>
        iri match {
          case types.`@list` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) List(ListType(valueTypes)) else List(ListType.datatype)) -> newTail
          case types.`@listset` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) List(ListSetType(valueTypes)) else List(ListSetType.datatype)) -> newTail
          case types.`@set` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) List(SetType(valueTypes)) else List(SetType.datatype)) -> newTail
          case types.`@vector` =>
            val (valueTypes, newTail) = getTypes(tail.drop(1))
            (if (valueTypes.nonEmpty) List(VectorType(valueTypes)) else List(VectorType.datatype)) -> newTail
          case types.`@map` =>
            val (keyTypes, newTail) = getTypes(tail.drop(1))
            if (!newTail.startsWith("(")) throw new Exception("map without second block")
            val (valueTypes, newTail2) = getTypes(newTail.drop(1))
            (if (keyTypes.nonEmpty || valueTypes.nonEmpty) List(MapType(keyTypes, valueTypes))
             else List(MapType.datatype)) -> newTail2
          case types.`@tuple` =>
            @tailrec
            def getT(tail: String, types: List[List[ClassType[Any]]]): (List[List[ClassType[Any]]], String) = {
              val (valueTypes, newTail) = getTypes(tail.drop(1))
              if (!newTail.startsWith("("))
                getT(newTail,
                     types :+ (if (valueTypes.nonEmpty) List(ListType(valueTypes)) else List(ListType.datatype)))
              else
                (types :+ (if (valueTypes.nonEmpty) List(ListType(valueTypes)) else List(ListType.datatype))) -> newTail
            }
            val (rangeTypes, newTail) = getT(tail, List())
            List(TupleType(rangeTypes)) -> newTail
          case _ =>
            scribe.error("cannot parse : " + iri)
            throw new Exception("cannot parse : " + iri)
        }
      case (iri, tail) if tail.startsWith(")") => get(iri).toList -> tail.dropWhile(_ == ')')
      case (iri, tail) if tail.startsWith("+") =>
        val (tailTypes, newTail) = getTypes(tail.drop(1))
        get(iri).toList ++ tailTypes -> newTail
    }
  }

  def get(iri: String): Option[DataType[Any]] = //TODO: .get (Task) instead of .cached
    ClassType.classtypes
      .cached(iri)
      .orElse(getTypes(iri) match {
        case (List(ct), "") =>
          Some(ct)
        case (List(ct), tail) =>
          scribe.warn(s"got type but tail is not empty, residu is: $tail")
          Some(ct)
        case (Nil, tail) =>
          scribe.warn(s"no classtype construct build for $iri, residu is: $tail")
          None
      })
      .asInstanceOf[Option[DataType[Any]]]
}

trait CollectionType[+T] extends StructuredType[T] {
  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  override val _properties: () => List[Property]              = () => List(CollectionType.keys.valueRange)
}
