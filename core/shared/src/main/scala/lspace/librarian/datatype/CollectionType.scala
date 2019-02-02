package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

//TODO: a note on collections: auto-merging resources by their iri can result in obsolete references from within collection structures.
//TODO: create a RefNode, RefEdge and RefValue to mitigate?
object CollectionType extends DataTypeDef[CollectionType[Iterable[Any]]] {

  val datatype = new CollectionType[Iterable[Any]] {
    val iri: String                                             = NS.types.`@collection`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@collection`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  }

//  lazy val ontology =
//    Ontology(NS.types.`@collection`, extendedClasses = List(StructuredValue.ontology))

  object keys {
    object valueRange
        extends PropertyDef(
          "@valueRange",
          "@valueRange",
          "A @valueRange",
          `@extends` = () => Property.default.`@range` :: Nil
        )
    lazy val valueRangeClassType: TypedProperty[List[ClassType[_]]] = valueRange + ListType(
      DataType.default.`@class` :: DataType.default.`@property` :: DataType.default.`@datatype` :: Nil)
//    lazy val valueRangeOntology: TypedProperty[Ontology]      = valueRange + DataType.default.`@class`
//    lazy val valueRangeProperty: TypedProperty[Property]      = valueRange + DataType.default.`@property`
//    lazy val valueRangeDatatype: TypedProperty[DataType[Any]] = valueRange + DataType.default.`@datatype`
  }
  override lazy val properties: List[Property] = keys.valueRange :: Nil //StructuredValue.properties
  trait Properties { //extends StructuredValue.Properties {
    lazy val valueRange: Property                                   = keys.valueRange
    lazy val valueRangeClassType: TypedProperty[List[ClassType[_]]] = keys.valueRangeClassType
//    lazy val valueRangeOntology: TypedProperty[Ontology]      = keys.valueRangeOntology
//    lazy val valueRangeProperty: TypedProperty[Property]      = keys.valueRangeProperty
//    lazy val valueRangeDatatype: TypedProperty[DataType[Any]] = keys.valueRangeDatatype
  }

  def apply[V](valueRange: List[ClassType[V]]) = new CollectionType[Iterable[V]] {
    val iri: String = s"${NS.types.`@collection`}/${valueRange.map(_.iri).sorted.mkString("+")}"
    //    override lazy val extendedClasses: List[DataType[Iterable[V]]] = List(StructuredValue[V]())
  }

  implicit def clsCollection[T]: ClassTypeable.Aux[CollectionType[T], Iterable[Any], CollectionType[Iterable[Any]]] =
    new ClassTypeable[CollectionType[T]] {
      type C  = Iterable[Any]
      type CT = CollectionType[Iterable[Any]]
      def ct: CT = datatype
    }
}

trait CollectionType[+T] extends StructuredType[T] {
  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  override val _properties: () => List[Property]              = () => List(CollectionType.keys.valueRange)
}
