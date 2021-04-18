package lspace.librarian.traversal

import lspace.datatype.ListType
import lspace.librarian.traversal.step.Step
import lspace.librarian.traversal.util.ResultMapper
import lspace.structure.{ClassType, Graph}
import shapeless.{HList, HNil}

object UntypedTraversal {
  implicit def listToHList(vector: Vector[_]): HList = vector.foldLeft[HList](HNil) { case (hl, e) => e :: hl }

//  case class ValidationError(message: String) extends Exception
//  case class ValidationSuccessBuilder(optimizationSuggestion: Option[String])
//  case class ValidationSuccess(optimizationSuggestion: String = "")

//  implicit class WithUnTypedTraversal(val untypedTraversal: UntypedTraversal)
//      extends WithTraversalStream[ClassType[Any], ClassType[Any], HList, Any] {
//    val traversal = untypedTraversal.toTyped
//  }
//  implicit class WithTraversalStreamUntyped(val traversal: UntypedTraversal) {

//    def validate(): Either[ValidationError, ValidationSuccess] =
//      traversal.steps match {
//        case Vector() =>
//          Right(
//            ValidationSuccess("Empty traversal is never eligible for execution and will result in an empty result."))
//        case steps =>
//          validateSteps(steps)
//      }
//
//    private def validateSteps(steps: Vector[Step]): Either[ValidationError, ValidationSuccess] = {
//      import scala.collection.immutable.::
//      steps.toList match {
//        case head :: tail =>
//          val result = tail.map {
//            case _ => Right(ValidationSuccessBuilder(None))
////            case step: FilterStep           => Right(ValidationSuccessBuilder(None))
////            case step: RearrangeBarrierStep => Right(ValidationSuccessBuilder(None))
//            case step => Left(ValidationError(s"step ${step.toString} was expected at the start of a segment"))
//          }
//          result.collect { case Left(error) => error } match {
//            case Nil =>
//              result.map(_.right.get) match {
//                case Nil => Right(ValidationSuccess())
//                case reports =>
//                  Right(ValidationSuccess(reports.flatMap(_.optimizationSuggestion) match {
//                    case Nil => ""
//                    case l   => l.mkString(" -and- ")
//                  }))
//              }
//            case errors => Left(ValidationError(errors.map(_.message).mkString(" -and- ")))
//          }
//        case Nil => Right(ValidationSuccess())
//      }
//    }
//  }

  def apply[ST <: ClassType[Any], ET <: ClassType[Any], Steps <: HList](
    traversal: Traversal[ST, ET, Steps]
  ): UntypedTraversal = new UntypedTraversal(traversal.stepsList.toVector)
}

case class UntypedTraversal private (steps: Vector[Step] = Vector()) {

//  def steps: List[Step]                                         = segments.flatMap(_.steps).toList
  def toTyped: Traversal[ClassType[Any], ClassType[Any], _ <: HList] = Traversal(steps)

  def withGraph[F[_]](graph: Graph)(implicit mapper: ResultMapper[F, ClassType[Any], ListType[List[Any]]]): mapper.FT =
    mapper(toTyped, graph).asInstanceOf[mapper.FT]

  def ++(traversal: UntypedTraversal): UntypedTraversal =
    this.copy(steps ++ traversal.steps)

//  lazy val toNode: Task[Node] = {
//    for {
//      node  <- DetachedGraph.nodes.create(Traversal.ontology)
//      steps <- Task.parSequence(steps.map(_.toNode).toVector)
//      e     <- node.addOut(Traversal.keys.stepsNode, steps)
////      segments <- Task.parSequence(segments.map(_.toNode))
////      _        <- node.addOut(Traversal.keys.segmentNode, segments)
//    } yield node
//  }
}
