package lspace.codec

import scala.collection.immutable.Map

object ExpandedMap {
  def apply[V](obj: Map[String, V])(implicit activeContext: ActiveContext): ExpandedMap[V] =
    new ExpandedMap(activeContext.expandKeys(obj))
}

class ExpandedMap[V](val obj: Map[String, V]) {
  def get(key: String)      = obj.get(key)
  def contains(key: String) = obj.contains(key)
  def size                  = obj.size
  def isEmpty               = obj.isEmpty
  def nonEmpty              = obj.nonEmpty
  def keys                  = obj.keys

  def -(key: String): ExpandedMap[V] = new ExpandedMap[V](obj - key)
}
