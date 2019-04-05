package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case object Dedup
    extends StepDef("Dedup",
                    "A dedup-step deduplicates traversers holding the same resource or result.",
                    `@extends` = () => GlobalFilterStep.ontology :: Nil)
    with StepWrapper[Dedup]
    with Dedup {

  def toStep(node: Node): Task[Dedup] = Task.now(this)

  object keys extends GlobalFilterStep.Properties
  override lazy val properties: List[Property] = GlobalFilterStep.properties
  trait Properties extends GlobalFilterStep.Properties

  lazy val toNode: Task[Node]      = DetachedGraph.nodes.create(ontology).memoizeOnSuccess
  override def prettyPrint: String = "dedup()"
}

trait Dedup extends GlobalFilterStep
