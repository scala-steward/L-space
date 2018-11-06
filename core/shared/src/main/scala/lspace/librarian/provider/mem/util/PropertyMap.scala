package lspace.librarian.provider.mem.util

import lspace.librarian.structure.{Edge, Property}

import scala.collection.mutable

case class PropertyMapEntry[T](subPropertyOf: mutable.HashSet[Property] = mutable.HashSet(),
                               edges: mutable.LinkedHashSet[Edge[T, _]] = mutable.LinkedHashSet[Edge[T, _]]())
    extends PropertyMap[T]()

object PropertyMap {
  def apply[T](): PropertyMap[T] = new PropertyMap[T]()
}
class PropertyMap[T]() {
  protected val map: mutable.HashMap[Property, PropertyMapEntry[T]] = mutable.HashMap[Property, PropertyMapEntry[T]]()

  def +=[V](kv: (Property, List[Edge[T, V]])): this.type = {
//    if(kv._2.nonEmpty) kv._1.extendedClasses
    map += kv._1 -> {
      val pme = map.getOrElse(kv._1, PropertyMapEntry[T]())
      pme.edges.clear()
      pme.edges ++= kv._2
      pme
    }
    this
  }

  def -=(edge: Edge[_, _]): this.type = ???

  def get(key: Property): List[Edge[T, _]] =
    map
      .get(key)
      .map(map => map.edges.toList ++ map.subPropertyOf.flatMap(get))
      .getOrElse(List())
}
