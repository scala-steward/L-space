package lspace.structure

import lspace.structure.util.ClassTypeable
import monix.eval.Task
import shapeless.<:!<

/** A partial in edge as helper to write triples
  *
  * @param from
  * @param key
  * @tparam T
  */
case class PartialInEdge[+T](from: Resource[T], key: Property) {
  def ---[V, V0, VT0 <: ClassType[_]](
    value: V
  )(implicit ev: V <:!< ClassType[_], dt: ClassTypeable.Aux[V, V0, VT0]): Task[Edge[V0, T]] =
    from.addIn(key, value)(ev, dt)
  def ---[V <: ClassType[_]](value: V): Task[Edge[Node, T]] =
    from.addIn(key, value)
}
