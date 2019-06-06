package lspace.librarian.traversal

import lspace.datatype.VectorType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil, LUBConstraint}

object Segment
    extends OntologyDef(lspace.NS.vocab.Lspace.+("librarian/TraversalSegment"),
                        Set(),
                        "TraversalSegment",
                        "A traversal-segment .. ") {

  object keys {

    object steps
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/TraversalSegment/steps",
          "steps",
          "The steps in a traversal-segment",
          `@range` = () => VectorType(Step.ontology :: Nil) :: Nil
        ) {}

    lazy val stepsNode: TypedProperty[Vector[Node]] = steps.property + VectorType(Step.ontology :: Nil)
  }

  override lazy val properties: List[Property] = keys.steps :: Nil

  trait Properties {
    lazy val `ns.l-space.eu/librarian/TraversalSegment/steps`: Property                    = keys.steps
    lazy val `ns.l-space.eu/librarian/TraversalSegment/steps@Node`: TypedKey[Vector[Node]] = keys.stepsNode
  }

  def apply(): Segment[HNil] = new Segment(HNil)
  def apply[Steps <: HList](steps: Steps)(implicit lub: LUBConstraint[Steps, Step]): Segment[Steps] =
    new Segment(steps)

  def toTraversalSegment(node: Node): Task[Segment[HList]] = {
    val types = node.labels

    for {
      steps0 <- Task
        .gather(node.out(Segment.keys.stepsNode).take(1).flatMap(_.toList).map(Step.toStep))
        .map(_.foldLeft[HList](HNil) {
          case (hlist, step) => step :: hlist
        })
    } yield new Segment(steps0)
  }
}

case class Segment[Steps <: HList] protected[lspace] (steps: Steps) {

  lazy val stepsList: List[Step] = steps.runtimeList.asInstanceOf[List[Step]].reverse
  def untyped: UntypedSegment    = UntypedSegment(stepsList.toVector)

  override def equals(o: Any): Boolean = o match {
    case traversalSegment: Segment[HList] @unchecked =>
      stepsList == traversalSegment.stepsList
  }

  lazy val toNode: Task[Node] = {
    for {
      node  <- DetachedGraph.nodes.create(Segment.ontology)
      steps <- Task.gather(stepsList.map(_.toNode).toVector)
      e     <- node.addOut(Segment.keys.stepsNode, steps)
    } yield node
  }

  def prettyPrint: String = {
    stepsList.map(_.prettyPrint).mkString(".")
  }
}
