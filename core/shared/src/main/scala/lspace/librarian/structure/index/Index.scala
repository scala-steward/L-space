package lspace.librarian.structure.index

import lspace.librarian.process.traversal.p.Eqv
import lspace.librarian.process.traversal.{EqP, P}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure.{ClassType, DataType, Property, Resource}

trait Index {
  def pattern: Vector[Set[_ <: ClassType[_]]]
//  def order: Vector[Option[ClassType[_]]]
  lazy val sortedPattern: Vector[Vector[_ <: ClassType[_]]] = pattern.map(_.toVector.sortBy(_.iri))

  lazy val id: Long = pattern.hashCode()

  /**
    * adds value-path to resources-path
    * @param pattern
    */
  def store(pattern: Vector[(Map[_ <: ClassType[_], List[Resource[_]]], Resource[_])]): Unit

  /**
    * searches for value-path-pattern in this index
    * @param values
    * @return list of applicable resource-paths
    */
  def find(values: Vector[Map[_ <: ClassType[_], List[P[_]]]]): List[Vector[Resource[_]]]

  /**
    * removes value-paths and purges resource-path when it is incomplete
    * @param path
    */
  def delete(path: Vector[(Map[_ <: ClassType[_], List[Resource[_]]], Resource[_])]): Unit

  def find[T](predicates: List[P[T]], datatype: DataType[T]): List[Resource[T]] = {
    find(Vector(Map(datatype -> predicates))).flatten
      .asInstanceOf[List[Resource[T]]]
  }

  def find[T](predicates: List[P[T]], property: Property): List[Resource[T]] = {
    find(Vector(Map(property -> predicates))).flatten
      .asInstanceOf[List[Resource[T]]]
  }
}
