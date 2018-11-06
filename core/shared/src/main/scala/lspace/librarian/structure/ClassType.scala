package lspace.librarian.structure

import java.time.{Instant, LocalDate, LocalTime}

import lspace.librarian.datatype._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.types.vector.{Geometry, Point}

object ClassType {

  def valueToOntologyResource[T](value: T): DataType[T] = {
    (value match {
      case r: Node        => DataType.default.nodeURLType
      case r: Edge[_, _]  => DataType.default.edgeURLType
      case r: Value[_]    => DataType.default.valueURLType
      case r: Ontology    => DataType.default.ontologyURLType
      case r: Property    => DataType.default.propertyURLType
      case r: DataType[_] => DataType.default.dataTypeURLType
      case r: IriResource => DataType.default.uRLType
      case v: String      => DataType.default.textType
      case v: Int         => DataType.default.intType
      case v: Double      => DataType.default.doubleType
      case v: Long        => DataType.default.longType
      case v: Instant     => DataType.default.dateTimeType
      case v: LocalDate   => DataType.default.dateType
      case v: LocalTime   => DataType.default.timeType
      case v: Boolean     => DataType.default.boolType
      case v: Point       => DataType.default.geopointType
      //      case v: Geometry => DataType.default.geoType
      //      case v: List[_] => DataType.default.listType()
      //      case v: ListSet[_] => DataType.default.listsetType()
      //      case v: Set[_] => DataType.default.setType()
      //      case v: Vector[_] => DataType.default.vectorType()
      //      case v: Map[_, _] => DataType.default.mapType()
      case _ => throw new Exception(s"not a known range ${value.getClass}")
    }).asInstanceOf[DataType[T]]
  }

  def default[T]: ClassType[T] = new ClassType[T] {
    type CT = ClassType[_]

    val iri: String                                    = ""
    val iris: Set[String]                              = Set()
    val label: Map[String, String]                     = Map()
    val comment: Map[String, String]                   = Map()
    val _extendedClasses: () => List[_ <: DataType[_]] = () => List()
    val _properties: () => List[Property]              = () => List()
    val base: Option[String]                           = None

    override def toString: String = s"classtype:$iri"
  }

  implicit def clsClasstype[T]: ClassTypeable.Aux[ClassType[T], T, ClassType[T]] = new ClassTypeable[ClassType[T]] {
    type C  = T
    type CT = ClassType[T]
    def ct: CT = default[T]
  }
}

trait ClassType[+T] extends IriResource {

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
