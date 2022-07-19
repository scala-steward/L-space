package lspace

import classtypes._

/** @tparam V
  *   value-type
  */
trait Value[+V] extends Resource[Value[V]]

case class OrphanValue[+V](v: V, context: ClassType[V]) extends Value[V]

opaque type LValue[name, +V] <: Value[V] = Value[V]
