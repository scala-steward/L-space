package lspace.librarian.process.traversal.step

import lspace.librarian.datatype.DataType
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._

object TimeLimit
    extends StepDef("TimeLimit",
                    "A timeLimit-step restricts the amount of time a traversal may run.",
                    () => EnvironmentStep.ontology :: Nil)
    with StepWrapper[TimeLimit] {

  def toStep(node: Node): TimeLimit = TimeLimit(node.out(TimeLimit.keys.durationTime).take(1).headOption)

  object keys extends EnvironmentStep.Properties {
    object duration
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/TimeLimit/duration",
          "duration",
          "The maximum time the underlaying traversal may take",
          `@range` = () => DataType.default.`@duration` :: Nil
        )
    val durationTime: TypedProperty[squants.time.Time] = duration.property + DataType.default.`@duration`
  }
  override lazy val properties: List[Property] = keys.duration :: EnvironmentStep.properties
  trait Properties extends EnvironmentStep.Properties {
    val duration     = keys.duration
    val durationTime = keys.durationTime
  }

  implicit def toNode(timeLimit: TimeLimit): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    timeLimit.time.foreach(time => node.addOut(keys.durationTime, time))
    node
  }
}

case class TimeLimit(time: Option[squants.time.Time] = None) extends EnvironmentStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    time.map(s"timeLimit(" + _.toString() + ")").getOrElse("timeLimit()")
}
