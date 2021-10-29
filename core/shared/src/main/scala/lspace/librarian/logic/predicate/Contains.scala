package lspace.librarian.logic.predicate

import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Contains
    extends PredicateDef("Contains", `@extends` = List(CollectionP.ontology))
    with PredicateWrapper[Contains[_]] {
  trait Helper[T] {
    def contains(avalue: Any, pvalue: T): Boolean
  }

  def toP(node: Node): Contains[_] = {
    val pvalue = node.out(CollectionP.keys.value).head
    Contains(pvalue)
  }

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](contains: Contains[T]): Task[Node] =
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(contains.pvalue), contains.pvalue)
    } yield node
}

case class Contains[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"contains($pvalue)"
}
