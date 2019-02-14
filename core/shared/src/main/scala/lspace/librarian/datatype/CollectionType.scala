package lspace.librarian.datatype

import lspace.NS
import lspace.NS.types
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

  object keys {
    object valueRange
        extends PropertyDef(
          "@valueRange",
          "@valueRange",
          "A @valueRange",
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

  private def collectionIri(iri: String): Option[DataType[_]] = {
    def iriToCollectionType(iri: String, tail: String): (ClassType[Any], String) = {
      iri match {
        case types.`@list` =>
          val (list, newTail) = getIris(tail, 1)
          ListType(list.head) -> newTail
        case types.`@listset` =>
          val (list, newTail) = getIris(tail, 1)
          ListSetType(list.head) -> newTail
        case types.`@set` =>
          val (list, newTail) = getIris(tail, 1)
          ListType(list.head) -> newTail
        case types.`@vector` =>
          val (list, newTail) = getIris(tail, 1)
          ListType(list.head) -> newTail
        case types.`@map` =>
          val (list, newTail) = getIris(tail, 2)
          MapType(list.head, list(1)) -> newTail
        case types.`@tuple2` =>
          val (list, newTail) = getIris(tail, 2)
          Tuple2Type(list.head, list(1)) -> newTail
        case types.`@tuple3` =>
          val (list, newTail) = getIris(tail, 3)
          Tuple3Type(list.head, list(1), list(2)) -> newTail
        case types.`@tuple4` =>
          val (list, newTail) = getIris(tail, 4)
          Tuple4Type(list.head, list(1), list(2), list(3)) -> newTail
        case _ =>
//          graph.ns.classtypes.cached(iri).getOrElse(throw FromJsonException(s"cannot decode or find $iri")) -> tail
          ClassType.classtypes.cached(iri).getOrElse(throw new Exception(s"cannot decode or find $iri")) -> tail
        //TODO: catch others
      }
    }
    def getIris(tail: String, parts: Int = 1): (List[List[ClassType[Any]]], String) = {
      val indexClosingParenthesis = tail.indexOf(')')
      val indexOpeningParenthesis = tail.indexOf('(')
      if (indexClosingParenthesis < indexOpeningParenthesis || indexOpeningParenthesis == -1) {
        val (tailClasstypes, tailIri) =
          if (parts > 1) getIris(tail.drop(indexClosingParenthesis + 2), parts - 1)
          else List[List[ClassType[_]]]() -> "" //get other parts
        (tail
          .take(indexClosingParenthesis)
          .split('+')
          .toList
//          .map(activeContext.expandIri)
          .map(get(_).getOrElse(throw new Exception("unknown @type"))) :: tailClasstypes) -> tailIri //concat
      } else {
        tail
          .take(indexClosingParenthesis)
          .split('+')
          .toList
//          .map(activeContext.expandIri)
        match { //split types until nested type @int+@double+@list/(
          case head :: Nil =>
            val (tpe, newTail) = iriToCollectionType(head.stripSuffix("/"), tail.drop(indexOpeningParenthesis + 1))
            val (tailTpes, newTail2) =
              if (parts > 2) getIris(newTail, parts - 2)
              else List[List[ClassType[_]]]() -> tail
            (List(tpe) :: tailTpes) -> newTail2
          case list =>
            val (tpe, newTail) =
              iriToCollectionType(list.last.stripSuffix("/"), tail.drop(indexOpeningParenthesis + 1))
            val (tailTpes, newTail2) =
              if (parts > 1 + list.size) getIris(newTail, parts - (1 + list.size))
              else List[List[ClassType[_]]]() -> tail
            (List((list
              .dropRight(1)
              .map(get(_).getOrElse(throw new Exception("unknown @type"))) :+ tpe) ::: tailTpes.head) ::: tailTpes.tail) -> newTail2
        }
      }
    }

    iri.replaceAll("\\/", "").split("\\(", 2).toList match {
      case List(iri, tail) =>
        Some(iri match {
          case types.`@list`    => iriToCollectionType(iri, tail)._1
          case types.`@listset` => iriToCollectionType(iri, tail)._1
          case types.`@set`     => iriToCollectionType(iri, tail)._1
          case types.`@vector`  => iriToCollectionType(iri, tail)._1
          case types.`@map`     => iriToCollectionType(iri, tail)._1
          case types.`@tuple2`  => iriToCollectionType(iri, tail)._1
          case types.`@tuple3`  => iriToCollectionType(iri, tail)._1
          case types.`@tuple4`  => iriToCollectionType(iri, tail)._1
        }).asInstanceOf[Option[DataType[Any]]]
    }
  }

  def get(iri: String): Option[ClassType[Any]] = //TODO: .get (Task) instead of .cached
    ClassType.classtypes
      .cached(iri)
      .orElse(collectionIri(iri))
}

trait CollectionType[+T] extends StructuredType[T] {
  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  override val _properties: () => List[Property]              = () => List(CollectionType.keys.valueRange)
}
