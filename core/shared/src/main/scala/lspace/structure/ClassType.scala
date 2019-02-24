package lspace.structure

import java.time.{Instant, LocalDate, LocalTime}

import lspace.datatype._
import lspace.structure.util.ClassTypeable
import lspace.types.vector._
import monix.eval.{Coeval, Task}

import scala.collection.immutable.ListSet

object ClassType {

  def valueToOntologyResource[T](value: T): DataType[T] = {
    (value match {
      case r: Node        => DataType.default.`@nodeURL`
      case r: Edge[_, _]  => DataType.default.`@edgeURL`
      case r: Value[_]    => DataType.default.`@valueURL`
      case r: Ontology    => DataType.default.`@class`
      case r: Property    => DataType.default.`@property`
      case r: DataType[_] => DataType.default.`@datatype`
      case r: IriResource => DataType.default.`@url`
      case v: String      => DataType.default.`@string`
      case v: Int         => DataType.default.`@int`
      case v: Double      => DataType.default.`@double`
      case v: Long        => DataType.default.`@long`
      case v: Instant     => DataType.default.`@datetime`
      case v: LocalDate   => DataType.default.`@date`
      case v: LocalTime   => DataType.default.`@time`
      case v: Boolean     => DataType.default.`@boolean`
      case v: Geometry =>
        v match {
          case v: Point         => DataType.default.`@geopoint`
          case v: MultiPoint    => DataType.default.`@geomultipoint`
          case v: Line          => DataType.default.`@geoline`
          case v: MultiLine     => DataType.default.`@geomultiline`
          case v: Polygon       => DataType.default.`@geopolygon`
          case v: MultiPolygon  => DataType.default.`@geomultipolygon`
          case v: MultiGeometry => DataType.default.`@geomultigeo`
          case _                => DataType.default.`@geo`
        }
      case v: Map[_, _]    => DataType.default.mapType() //TODO: recursively map nested values to map -> toList.distinct ?
      case v: ListSet[_]   => DataType.default.listsetType()
      case v: List[_]      => DataType.default.listType()
      case v: Set[_]       => DataType.default.setType()
      case v: Vector[_]    => DataType.default.vectorType()
      case v: (_, _)       => DataType.default.tuple2Type()
      case v: (_, _, _)    => DataType.default.tuple3Type()
      case v: (_, _, _, _) => DataType.default.tuple4Type()
      case _ =>
        throw new Exception(s"not a known range ${value.getClass}")
    }).asInstanceOf[DataType[T]]
  }

  def build(node: Node): Task[Coeval[ClassType[_]]] = {
    if (node.hasLabel(Ontology.ontology).nonEmpty) {
      if (node.hasLabel(DataType.ontology).nonEmpty) DataType.build(node)
      else Ontology.build(node)
    } else if (node.hasLabel(Property.ontology).nonEmpty) {
      Property.build(node)
    } else {
      Task.raiseError(new Exception(s"${node.iri} is does not look like a classtype"))
    }
  }

  object classtypes {
    def get(iri: String): Option[Task[Coeval[ClassType[_]]]] =
      Ontology.ontologies.get(iri).orElse(Property.properties.get(iri)).orElse(DataType.datatypes.get(iri))

    def cached(iri: String): Option[ClassType[_]] =
      Ontology.ontologies.cached(iri).orElse(Property.properties.cached(iri)).orElse(DataType.datatypes.cached(iri))
  }

  def makeT[T]: ClassType[T] = new ClassType[T] {

    val iri: String                                    = ""
    val iris: Set[String]                              = Set()
    val label: Map[String, String]                     = Map()
    val comment: Map[String, String]                   = Map()
    val _extendedClasses: () => List[_ <: DataType[_]] = () => List()
    val _properties: () => List[Property]              = () => List()
    val base: Option[String]                           = None

    override def toString: String = s"classtype:$iri"
  }
  //helper, empty iri is used to recognize and filter out this classtype
  lazy val stubAny: ClassType[Any] = makeT[Any]

  //helper, empty iri is used to recognize and filter out this classtype
  lazy val stubNothing: ClassType[Nothing] = makeT[Nothing]

//  implicit def clsClasstype[T]: ClassTypeable.Aux[ClassType[T], T, ClassType[T]] = new ClassTypeable[ClassType[T]] {
//    type C  = T
//    type CT = ClassType[T]
//    def ct: CT = default[T]
//  }
}

/**
  *
  * @tparam T
  */
trait ClassType[+T] extends IriResource {

  def iris: Set[String]

  protected def _properties: () => List[Property]

  protected def _extendedClasses: () => List[_ <: ClassType[_]]

  //  def extendedClasses: List[ClassType[_]] // = out(DataType.default.EXTENDS).collect { case node: Node => node }.map(ClassType.wrap).asInstanceOf[List[ClassType[_]]]
  /**
    * TODO: deprecate this method by implementing Ontology hierarchy
    * @param classType
    * @return
    */
  def `extends`(classType: ClassType[_]): Boolean = {
    if (extendedClasses.contains(classType)) true
    else {
      var result: Boolean = false
      val oIt             = extendedClasses.reverseIterator
      while (oIt.hasNext && !result) {
        result = oIt.next().`extends`(classType)
      }
      result
    }
  }

  def property(iri: String): Option[Property] = {
    properties
      .find(_.iri == iri)
      .orElse {
        var result: Option[Property] = None
        val oIt                      = extendedClasses.reverseIterator
        while (oIt.hasNext && result.isEmpty) {
          result = oIt.next().property(iri)
        }
        result
      }
  }

  lazy val properties: Set[Property] = _properties().toSet ++ extendedClasses.flatMap(_.properties)

  /**
    * TODO: create hash-tree for faster evaluation
    * @return
    */
  lazy val extendedClasses: List[_ <: ClassType[_]] = _extendedClasses()

  def label: Map[String, String]

  def comment: Map[String, String]

  def base: Option[String]
}
