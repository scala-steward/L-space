package lspace.structure

import lspace.structure.util.ClassTypeable
import shapeless.<:!<

/** A partial out edge as helper to write triples
  *
  * @param from
  * @param key
  * @tparam T
  */
case class PartialOutEdge[+T](from: Resource[T], key: Property) {
  def -->[V, V0, VT0 <: ClassType[_]](value: V)(implicit ev: V <:!< ClassType[_],
                                                dt: ClassTypeable.Aux[V, V0, VT0]): Edge[T, V0] =
    from.addOut(key, value)(ev, dt)
  def -->[V <: ClassType[_]](value: V): Edge[T, Node] =
    from.addOut(key, value)
  def ---[V, R[T] <: Resource[T]](value: R[V]): (Edge[T, V], Edge[V, T]) = from.addBoth(key, value)
}
