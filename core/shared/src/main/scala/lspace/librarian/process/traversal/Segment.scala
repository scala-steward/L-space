package lspace.librarian.process.traversal

import lspace.librarian.datatype.VectorType
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
import shapeless.{HList, HNil, LUBConstraint}

object Segment
    extends OntologyDef(lspace.NS.vocab.Lspace.+("librarian/TraversalSegment"),
                        Set(),
                        "TraversalSegment",
                        "A traversal-segment .. ") {

  object keys {

    object step
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/TraversalSegment/step",
          "step",
          "A step in a traversal",
          `@range` = () => VectorType(Step.ontology :: Nil) :: Nil
        ) {}

    lazy val stepNode: TypedProperty[Vector[Node]] = step.property + VectorType(Step.ontology :: Nil)
  }

  override lazy val properties: List[Property] = keys.step :: Nil

  trait Properties {
    lazy val `ns.l-space.eu/librarian/TraversalSegment/step`: Property                    = keys.step
    lazy val `ns.l-space.eu/librarian/TraversalSegment/step@Node`: TypedKey[Vector[Node]] = keys.stepNode
  }

  def apply(): Segment[HNil] = new Segment(HNil)
  def apply[Steps <: HList](steps: Steps)(implicit lub: LUBConstraint[Steps, Step]): Segment[Steps] =
    new Segment(steps)

  def toTraversalSegment(node: Node): Segment[HList] = {
    val types = node.labels

    val steps0 = node.out(Segment.keys.stepNode).take(1).flatMap(_.toList).foldLeft[HList](HNil) {
      case (hlist, node) => Step.toStep(node) :: hlist
    }

    new Segment(steps0)
  }
}

case class Segment[Steps <: HList] protected[lspace] (steps: Steps) {

  lazy val stepsList: List[Step] = steps.runtimeList.asInstanceOf[List[Step]].reverse

  override def equals(o: Any): Boolean = o match {
    case traversalSegment: Segment[HList] =>
      stepsList == traversalSegment.stepsList
  }

  lazy val toNode: Node = {
    val node0 = DetachedGraph.nodes.create(Segment.ontology)
    node0.addOut(Segment.keys.stepNode, stepsList.map(_.toNode).toVector)
    node0
  }

  def prettyPrint: String = {
    stepsList.map(_.prettyPrint).mkString(".")
  }
}
