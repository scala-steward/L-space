package lspace.librarian.structure

import lspace.librarian.process.traversal.helper.ClassTypeable
import shapeless.<:!<

case class PartialInEdge[T](from: Resource[T], key: Property) {
  def ---[V, V0, VT0 <: ClassType[_]](value: V)(implicit ev: V <:!< ClassType[_],
                                                dt: ClassTypeable.Aux[V, V0, VT0]): Edge[V0, T] =
    from.addIn(key, value)(ev, dt)
  def ---[V <: ClassType[_]](value: V): Edge[Node, T] =
    from.addIn(key, value)
  //  def -*-[V](values: V*): List[Edge[V, T]] = from.addIns(key, values.toList)
}
