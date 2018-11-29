package lspace.librarian.datatype

import lspace.NS
import lspace.librarian.structure.{DataType, Graph}

object GraphType {
  val default: GraphType[Graph] = new GraphType[Graph] { type Out = Graph }

  def apply[T <: Graph] = new GraphType[T] {}
}

trait GraphType[+T <: Graph] extends DataType[T] {
  val iri: String = NS.types.`@graph`
}
