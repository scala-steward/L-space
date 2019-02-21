package lspace.structure.index

import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal.{Traversal, UntypedTraversal}
import lspace.provider.detached.DetachedGraph
import lspace.structure.OntologyDef
import lspace.structure._
import lspace.structure.index.shape.Shape

object Index extends OntologyDef(lspace.NS.vocab.Lspace + s"Index", Set(), "Index", "An index ...") {
  object keys {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "Index/segment",
          "segment",
          "The traversal segment containing only FilterStep's and RearrangeStep's (e.g. Order) " +
            "and all segments start with an Out-step (except for the first segment)",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalNode: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal.property :: Nil
  trait Properties {
    val traversal     = keys.traversal
    val traversalNode = keys.traversalNode
  }

  implicit def toNode[I <: Index](index: I): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    index.traversal.segments.map(_.toNode).foreach(node.addOut(keys.traversalNode, _))
    node
  }
}

trait Index {

  lazy val toNode: Node = this

  /**
    * A traversal-pattern only Segments containing FilterStep's and RearrangeSteps separated by an Out-step
    * @return
    */
  def traversal: UntypedTraversal
//  def order: Vector[Option[ClassType[_]]]
//  lazy val sortedPattern: Vector[Vector[Property]] = pattern.map(_.toVector.sortBy(_.iri))

  lazy val id: Long = traversal.hashCode()

  /** TODO: add edge??? as pointer to changed datapoint to be used for incremental updates?
    * adds value-path to resources-path
    * @param pattern
    */
  def store(shape: Shape): Unit

  /**
    * searches for value-path-pattern in this index
    * @param values
    * @return list of applicable resource-paths
    */
  def find(values: Vector[Map[Property, List[P[_]]]]): List[Shape]

  /**
    * removes value-paths and purges resource-path when it is incomplete
    * @param path
    */
  def delete(shape: Shape): Unit
}