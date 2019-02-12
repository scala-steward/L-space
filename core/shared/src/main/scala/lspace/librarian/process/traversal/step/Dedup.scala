package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

case object Dedup
    extends StepDef("Dedup",
                    "A dedup-step deduplicates traversers holding the same resource or result.",
                    `@extends` = () => GlobalFilterStep.ontology :: Nil)
    with StepWrapper[Dedup]
    with Dedup {

  def toStep(node: Node): Dedup = this

  object keys extends GlobalFilterStep.Properties
  override lazy val properties: List[Property] = GlobalFilterStep.properties
  trait Properties extends GlobalFilterStep.Properties

  def toNode: Node                 = DetachedGraph.nodes.create(ontology)
  override def prettyPrint: String = "dedup()"
}

trait Dedup extends GlobalFilterStep
