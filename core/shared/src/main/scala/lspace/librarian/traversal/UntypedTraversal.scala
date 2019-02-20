package lspace.librarian.traversal

import lspace.librarian.traversal.Traversal.{keys, ontology, WithTraversalStream}
import lspace.provider.detached.DetachedGraph
import lspace.structure.{ClassType, Graph, Node}
import monix.eval.Task
import shapeless.{HList, HNil}

object UntypedTraversal {
  implicit def listToHList(vector: Vector[_]): HList = vector.foldLeft[HList](HNil) { case (hl, e) => e :: hl }

  case class ValidationError(message: String) extends Exception
  case class ValidationSuccessBuilder(optimizationSuggestion: Option[String])
  case class ValidationSuccess(optimizationSuggestion: String)

  implicit class WithUnTypedTraversal(val untypedTraversal: UntypedTraversal)
      extends WithTraversalStream[ClassType[Any], ClassType[Any], HList, Any] {
    val traversal = untypedTraversal.toTyped
  }
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

    private def validateSegment[Steps <: HList](
        segment: Segment[Steps]): Either[ValidationError, ValidationSuccessBuilder] = {
      import scala.collection.immutable.::
      segment.stepsList match {
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

case class UntypedTraversal(segments: Vector[Segment[HList]] = Vector()) {

  def steps: List[Step]                                         = segments.flatMap(_.stepsList).toList
  def toTyped: Traversal[ClassType[Any], ClassType[Any], HList] = Traversal[Any, Any](steps.toVector)

  lazy val toNode: Node = {
    val node0 = DetachedGraph.nodes.create(ontology)
//    segments.map(_.toNode).foreach(node0.addOut(keys.segmentNode, _))
    node0.addOut(keys.segmentNode, segments.map(_.toNode))
    node0
  }
}
