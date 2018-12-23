package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object TimeLimit
    extends StepDef("TimeLimit",
                    "A timeLimit-step restricts the amount of time a traversal may run.",
                    () => EnvironmentStep.ontology :: Nil)
    with StepWrapper[TimeLimit] {

  def wrap(node: Node): TimeLimit = node match {
    case node: TimeLimit => node
    case _               => TimeLimit(node.out(TimeLimit.keys.durationTime).take(1).headOption, node)
  }

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

  def apply(time: Option[squants.time.Time] = None): TimeLimit = {
    val node = DetachedGraph.nodes.create(ontology)

    time.foreach(time => node.addOut(keys.durationTime, time))
    TimeLimit(time, node)
  }

}

case class TimeLimit private (time: Option[squants.time.Time], override val value: Node)
    extends WrappedNode(value)
    with EnvironmentStep {
  override def prettyPrint: String =
    time.map(s"timeLimit(" + _.toString() + ")").getOrElse("timeLimit()")
}
