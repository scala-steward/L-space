package lspace.datatype

import lspace.NS
import lspace.structure.util.ClassTypeable
import lspace.structure._

object NodeURLType extends DataTypeDef[NodeURLType[Node]] {

  lazy val datatype = new NodeURLType[Node] {
    val iri: String                = NS.types.`@nodeURL`
    override val iris: Set[String] = Set(NS.types.`@nodeURL`)
    labelMap ++= Map("en" -> NS.types.`@nodeURL`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(IriType.datatype)
  }

  object keys extends IriType.Properties
  override lazy val properties: List[Property] = IriType.properties
  trait Properties extends IriType.Properties

  implicit def defaultT[T]: ClassTypeable.Aux[NodeURLType[T], T, NodeURLType[T]] =
    new ClassTypeable[NodeURLType[T]] {
      type C  = T
      type CT = NodeURLType[T]
      def ct: CT = apply[T]
    }

  def apply[T]: NodeURLType[T] = new NodeURLType[T] {
    val iri: String = NS.types.`@nodeURL`
    labelMap ++= Map("en" -> NS.types.`@nodeURL`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(IriType.datatype)
  }
}

trait NodeURLType[+T] extends IriType[T]

object EdgeURLType extends DataTypeDef[EdgeURLType[Edge[Any, Any]]] {

  lazy val datatype = new EdgeURLType[Edge[Any, Any]] {
    val iri: String = NS.types.`@edgeURL`
    labelMap ++= Map("en" -> NS.types.`@edgeURL`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(IriType.datatype)
  }

  object keys extends IriType.Properties
  override lazy val properties: List[Property] = IriType.properties
  trait Properties extends IriType.Properties

  implicit def defaultT[T <: Edge[_, _]]: ClassTypeable.Aux[EdgeURLType[T], T, EdgeURLType[T]] =
    new ClassTypeable[EdgeURLType[T]] {
      type C  = T
      type CT = EdgeURLType[T]
      def ct: CT = apply[T]
    }

  def apply[T <: Edge[_, _]]: EdgeURLType[T] = new EdgeURLType[T] {
    val iri: String = NS.types.`@edgeURL`
    labelMap ++= Map("en" -> NS.types.`@edgeURL`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(IriType.datatype)
  }
}

trait EdgeURLType[+T] extends IriType[T]

object ValueURLType extends DataTypeDef[ValueURLType[Any]] {

  lazy val datatype = new ValueURLType[Any] {
    val iri: String = NS.types.`@valueURL`
    labelMap ++= Map("en" -> NS.types.`@valueURL`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(IriType.datatype)
  }

  object keys extends IriType.Properties
  override lazy val properties: List[Property] = IriType.properties
  trait Properties extends IriType.Properties

  implicit def defaultT[T <: Value[_]]: ClassTypeable.Aux[ValueURLType[T], T, ValueURLType[T]] =
    new ClassTypeable[ValueURLType[T]] {
      type C  = T
      type CT = ValueURLType[T]
      def ct: CT = apply[T]
    }

  def apply[T]: ValueURLType[T] = new ValueURLType[T] {
    val iri: String = NS.types.`@valueURL`
    labelMap ++= Map("en" -> NS.types.`@valueURL`)
    override protected def _extendedClasses: List[ClassType[Any]] = List(IriType.datatype)
  }
}

trait ValueURLType[+T] extends IriType[T]
