package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object NodeURLType {
  def nodeType[T]: NodeURLType[T] = new NodeURLType[T] { type Out = T; type CT = NodeURLType[T] }
//  implicit def classTypeable[T <: Node]: ClassTypeable[NodeURLType[T]] = new ClassTypeable[NodeURLType[T]] {
//    type CT = NodeURLType[T]
//    def ct: CT = nodeType[T]
//  }
  def default: NodeURLType[Node] = nodeType[Node]

  implicit def defaultT[T]: ClassTypeable.Aux[NodeURLType[T], T, NodeURLType[T]] =
    new ClassTypeable[NodeURLType[T]] {
      type C  = T
      type CT = NodeURLType[T]
      def ct: CT = NodeURLType.nodeType[T]
    }
}

trait NodeURLType[+T] extends IriType[T] {
  val iri: String = NS.types.`@nodeURL`
}

object EdgeURLType {
  def edgeUrlType[T <: Edge[_, _]]: EdgeURLType[T] = new EdgeURLType[T] { type Out = T; type CT = EdgeURLType[T] }
//  implicit def classTypeable[S, E, T <: Edge[S, E]]: ClassTypeable[EdgeURLType[T]] = new ClassTypeable[EdgeURLType[T]] {
//    type CT = EdgeURLType[T]
//    def ct: CT = edgeUrlType[T]
//  }
  def default: EdgeURLType[Edge[Any, Any]] = edgeUrlType[Edge[Any, Any]]

  implicit def defaultT[T <: Edge[_, _]]: ClassTypeable.Aux[EdgeURLType[T], T, EdgeURLType[T]] =
    new ClassTypeable[EdgeURLType[T]] {
      type C  = T
      type CT = EdgeURLType[T]
      def ct: CT = EdgeURLType.edgeUrlType[T]
    }
}
//object EdgeURLType extends EdgeURLType[Any, Any] {
//  def apply[S, E] = new EdgeURLType[S, E] {}
//
//  implicit def dt[T <: EdgeURLType[_, _]](implicit ev: T <:< EdgeURLType[_, _]) = DataType.urlType[T]
////  implicit def dt[T, CT[Z] <: StructuredValue[Z]](implicit ev: CT[T] <:< StructuredValue[T]) = DataType.urlType[CT[T]]
//  implicit def default = EdgeURLType
//}
trait EdgeURLType[+T] extends IriType[T] {
  val iri: String = NS.types.`@edgeURL`
}

object ValueURLType {
  def valueType[T <: Value[_]]: ValueURLType[T] = new ValueURLType[T] { type Out = T; type CT = ValueURLType[T] }
//  implicit def classTypeable[T <: Value[_]]: ClassTypeable[ValueURLType[T]] = new ClassTypeable[ValueURLType[T]] {
//    type CT = ValueURLType[T]
//    def ct: CT = valueType[T]
//  }
  def default: ValueURLType[Value[Any]] = valueType[Value[Any]]

  implicit def defaultT[T <: Value[_]]: ClassTypeable.Aux[ValueURLType[T], T, ValueURLType[T]] =
    new ClassTypeable[ValueURLType[T]] {
      type C  = T
      type CT = ValueURLType[T]
      def ct: CT = ValueURLType.valueType[T]
    }
}

trait ValueURLType[+T] extends IriType[T] {
  val iri: String = NS.types.`@valueURL`
}
