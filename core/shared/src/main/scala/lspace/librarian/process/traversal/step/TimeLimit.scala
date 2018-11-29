package lspace.librarian.process.traversal.step

import java.time.Instant

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.types._

object TimeLimit extends StepDef("TimeLimit") with StepWrapper[TimeLimit] {

  def wrap(node: Node): TimeLimit = node match {
    case node: TimeLimit => node
    case _               => TimeLimit(node.out(TimeLimit.keys.durationTime).take(1).headOption, node)
  }

  object keys {
    private val durationNode = MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/TimeLimit/duration")
    durationNode.addLabel(Property.ontology)
    durationNode --- Property.default.`@label` --> "duration" --- Property.default.`@language` --> "en"
    durationNode --- Property.default.`@comment` --> "The maximum time the underlaying traversal may take" --- Property.default.`@language` --> "en"
    durationNode --- Property.default.`@range` --> DataType.default.`@datetime`

    lazy val duration: Property                        = Property(durationNode)
    val durationTime: TypedProperty[squants.time.Time] = duration + DataType.default.`@duration`
  }

  def apply(time: Option[squants.time.Time] = None): TimeLimit = {
    val node = DetachedGraph.nodes.create(ontology)

    time.foreach(time => node.addOut(keys.durationTime, time))
    TimeLimit(time, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.duration
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class TimeLimit private (time: Option[squants.time.Time], override val value: Node)
    extends WrappedNode(value)
    with EnvironmentStep {
  override def prettyPrint: String =
    time.map(s"timeLimit(" + _.toString() + ")").getOrElse("timeLimit()")
}
