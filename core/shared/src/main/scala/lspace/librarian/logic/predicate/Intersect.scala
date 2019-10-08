package lspace.librarian.logic.predicate

import lspace.librarian.logic.predicate.P._
import lspace.librarian.logic.predicate._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Intersect
    extends PredicateDef("Intersect", `@extends` = () => CollectionP.ontology :: Nil)
    with PredicateWrapper[Intersect[_]] {

  def toP(node: Node): Intersect[_] = {
    val pvalue = node.out(CollectionP.keys.value).head
    Intersect(pvalue)
  }

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  implicit def toNode[T](p: Intersect[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, ClassType.detect(p.pvalue), p.pvalue)
    } yield node
  }
}

case class Intersect[+T](pvalue: T) extends CollectionP[T] {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"intersect($pvalue)"
}
