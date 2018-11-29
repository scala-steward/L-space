package lspace.lgraph.index

import lspace.lgraph.LGraph
import lspace.librarian.process.traversal.P
import lspace.librarian.structure.{ClassType, DataType, Resource}
import lspace.librarian.structure.index.Index

import scala.collection.mutable

object LIndex {
  def apply(pattern: DataType[_], graph: LGraph): LIndex                    = new LIndex(Vector(Set(pattern)), graph)
  def apply(pattern: Vector[Set[_ <: ClassType[_]]], graph: LGraph): LIndex = new LIndex(pattern, graph)
}

class LIndex(val pattern: Vector[Set[_ <: ClassType[_]]], graph: LGraph) extends Index {
  private val data: mutable.HashMap[Vector[Resource[_]], Vector[Vector[Vector[Resource[_]]]]] =
    mutable.HashMap[Vector[Resource[_]], Vector[Vector[Vector[Resource[_]]]]]()

  def store(path: Vector[(Map[_ <: ClassType[_], List[Resource[_]]], Resource[_])]): Unit = synchronized {
    val pathPattern = path.map(_._1.toList.sortBy(_._1.iri).map(_._1).toSet)
    if (pathPattern == pattern) {
      val pathValues = path.map(_._1.toVector.sortBy(_._1.iri).map(_._2).toVector)

      val pathResources = path.map(_._2)
      val newVector = data.getOrElse(pathResources, Vector()) zip pathValues map {
        case (a, b) => a zip b map { case (a, b) => a ++ b } //filter (_.nonEmpty)
      }
      data += pathResources -> newVector
    }
  }

  def find(values: Vector[Map[_ <: ClassType[_], List[P[_]]]]): List[Vector[Resource[_]]] = {
    val pathValues  = values.map(_.toList.sortBy(_._1.iri).map(_._2.toList))
    val pathPattern = values.map(_.toVector.sortBy(_._1.iri).map(_._1))
    if (pathPattern == pattern) {
      data.collect {
        case (path, data) if pathValues zip data forall {
              case (p, d) => p zip d forall { case (p, d) => p.forall(p => d.map(_.value).exists(p.assert)) }
            } =>
          path
      }.toList
    } else List()
  }

  def delete(path: Vector[(Map[_ <: ClassType[_], List[Resource[_]]], Resource[_])]): Unit = {
    val pathPattern = path.map(_._1.toList.sortBy(_._1.iri).map(_._1).toSet)
    if (pathPattern == pattern) {
      val pathValues = path.map(_._1.toVector.sortBy(_._1.iri).map(_._2).toVector)

      val pathResources = path.map(_._2)
      val newVector = data.getOrElse(pathResources, Vector()) zip pathValues map {
        case (a, b) => a zip b map { case (a, b) => a.filterNot(b.contains) } //filter (_.nonEmpty)
      }
      if (newVector.exists(_.exists(_.isEmpty))) data -= pathResources
      else data += pathResources -> newVector
    }
  }
}
