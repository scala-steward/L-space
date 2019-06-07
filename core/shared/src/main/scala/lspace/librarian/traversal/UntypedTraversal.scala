package lspace.librarian.traversal

import lspace.librarian.task.Guide
import lspace.provider.detached.DetachedGraph
import lspace.structure.{ClassType, Graph, Node}
import monix.eval.Task
import shapeless.{HList, HNil}

object UntypedTraversal {
  implicit def listToHList(vector: Vector[_]): HList = vector.foldLeft[HList](HNil) { case (hl, e) => e :: hl }

  case class ValidationError(message: String) extends Exception
  case class ValidationSuccessBuilder(optimizationSuggestion: Option[String])
  case class ValidationSuccess(optimizationSuggestion: String)

//  implicit class WithUnTypedTraversal(val untypedTraversal: UntypedTraversal)
//      extends WithTraversalStream[ClassType[Any], ClassType[Any], HList, Any] {
//    val traversal = untypedTraversal.toTyped
//  }
  implicit class WithTraversalStreamUntyped(val traversal: UntypedTraversal) {

    def validate(): Either[ValidationError, ValidationSuccess] =
      traversal.segments match {
        case Vector() =>
          Right(
            ValidationSuccess("Empty traversal is never eligible for execution and will result in an empty result."))
        case segments =>
          val validationResults = segments.map { segment =>
            validateSegment(segment)
          }
          val errors = validationResults.collect { case Left(error) => error }
          if (errors.nonEmpty) {
            Left(ValidationError(errors.map(_.message).mkString(" -and- ")))
          } else
            Right(
              ValidationSuccess(
                validationResults
                  .collect { case Right(report) => report }
                  .flatMap(_.optimizationSuggestion)
                  .mkString(" -and- ")))
      }

    private def validateSegment(segment: UntypedSegment): Either[ValidationError, ValidationSuccessBuilder] = {
      import scala.collection.immutable.::
      segment.steps.toList match {
        case head :: tail =>
          val result = tail.map {
            case step: FilterStep           => Right(ValidationSuccessBuilder(None))
            case step: RearrangeBarrierStep => Right(ValidationSuccessBuilder(None))
            case step                       => Left(ValidationError(s"step ${step.toString} was expected at the start of a segment"))
          }
          result.collect { case Left(error) => error } match {
            case Nil =>
              result.map(_.right.get) match {
                case Nil => Right(ValidationSuccessBuilder(None))
                case reports =>
                  Right(ValidationSuccessBuilder(reports.flatMap(_.optimizationSuggestion) match {
                    case Nil => None
                    case l   => Some(l.mkString(" -and- "))
                  }))
              }
            case errors => Left(ValidationError(errors.map(_.message).mkString(" -and- ")))
          }
        case Nil => Right(ValidationSuccessBuilder(None))
      }
    }
  }
}

case class UntypedTraversal(segments: Vector[UntypedSegment] = Vector()) {

  def steps: List[Step]                                         = segments.flatMap(_.steps).toList
  def toTyped: Traversal[ClassType[Any], ClassType[Any], HList] = Traversal(steps.toVector)

  def withGraph[F[_]](graph: Graph)(implicit guide: Guide[F], mapper: Mapper[F, HNil, Any]): mapper.FT =
    mapper(toTyped, graph).asInstanceOf[mapper.FT]

  def ++(traversal: UntypedTraversal): UntypedTraversal = {
    traversal.steps.headOption match {
      case Some(step: MoveStep) =>
        this.copy(segments ++ traversal.segments)
      case Some(step) =>
        this.copy(
          segments.dropRight(1) ++ segments.lastOption.map(
            _ ++ traversal.segments.headOption.getOrElse(UntypedSegment())) ++ traversal.segments.tail)
      case None => this
    }
  }

  lazy val toNode: Task[Node] = {
    for {
      node     <- DetachedGraph.nodes.create(Traversal.ontology)
      segments <- Task.gather(segments.map(_.toNode))
      _        <- node.addOut(Traversal.keys.segmentNode, segments)
    } yield node
  }
}

case class UntypedSegment(steps: Vector[Step] = Vector()) {

  def toTyped: Segment[HList] = new Segment(steps.reverse.foldLeft[HList](HNil) { case (r, s) => s :: r })

  def ++(segment: UntypedSegment): UntypedSegment = this.copy(steps ++ segment.steps)

  lazy val toNode: Task[Node] = {
    for {
      node  <- DetachedGraph.nodes.create(Segment.ontology)
      steps <- Task.gather(steps.map(_.toNode).toVector)
      e     <- node.addOut(Segment.keys.stepsNode, steps)
    } yield node
  }

  def prettyPrint: String = {
    steps.map(_.prettyPrint).mkString(".")
  }
}
