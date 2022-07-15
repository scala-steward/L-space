package lspace

import classtypes._

/** @tparam V
  *   value-type
  */
open class Value[+V] extends Resource[Value[V]]

case class OrphanValue[+V](v: V, context: ClassType[V]) extends Value[V]
