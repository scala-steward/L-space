package lspace

/** @tparam V
  *   value-type
  */
open class Value[+V](v: V) extends Resource[Value[V]]
