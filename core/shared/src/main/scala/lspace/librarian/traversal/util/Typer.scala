package lspace.librarian.traversal.util

import lspace.datatype.DataType.default.`@long`
import lspace.datatype._
import lspace.librarian.traversal.step._
import lspace.librarian.traversal._
import lspace.structure.{ClassType, Edge, Node}
import shapeless.{HList, HNil}

import scala.annotation.tailrec

object Typer {

  @tailrec
  def retype(steps: Vector[Step], traversal: Traversal[ClassType[Any], ClassType[Any], _ <: HList] = Traversal.apply())
  : Traversal[ClassType[Any], ClassType[Any], _ <: HList] =
    steps match {
      case Vector() => traversal
      case (step: TraverseStep) +: steps =>
        val typedTraversal = step match {
          case step: Constant[_] =>
            new Traversal(step :: traversal.steps)(traversal.st, step.label)
          case step: From => //TODO: create EdgeUrlType with From and To type information
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
          case step: To => //TODO: create EdgeUrlType with From and To type information
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
          case step: Id => new Traversal(step :: traversal.steps)(traversal.st, `@long`)
        }
        retype(steps, typedTraversal)
      case (step: ResourceStep) +: steps =>
        val typedTraversal = step match {
          case step: N => new Traversal(step :: traversal.steps)(traversal.st, Node.nodeUrl)
          case step: E => new Traversal(step :: traversal.steps)(traversal.st, Edge.edgeUrl)
          case step: V =>
            new Traversal(step :: traversal.steps)(traversal.st, ValueURLType.datatype)
          case step: R =>
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
          case step: GraphStep =>
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
        }
        retype(steps, typedTraversal)
      case (step: BranchStep) +: steps =>
        val typedTraversal = step match {
          case step: BranchStep =>
            step match {
              case step: MoveStep =>
                step match {
                  case step: Out =>
                    new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
                  case step: OutE =>
                    new Traversal(step :: traversal.steps)(traversal.st, Edge.edgeUrl)
                  case step: In =>
                    new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
                  case step: InE =>
                    new Traversal(step :: traversal.steps)(traversal.st, Edge.edgeUrl)
                }
              case step: Choose[_, _] =>
                val typedStep = Choose(
                  step.by.retype(traversal.et, traversal.et),
                  step.right.retype(traversal.et, traversal.et),
                  step.left.retype(traversal.et, traversal.et)
                )
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.right.et + typedStep.left.et)
              case step: Coalesce[_, _] =>
                val typedStep = Coalesce(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversals.map(_.et).reduce(_ + _))
              case step: Local[_, _] =>
                val typedStep = Local(step.traversal.retype(traversal.et, traversal.et))
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversal.et)
              case step: Repeat[_] =>
                val typedStep = Repeat(step.traversal.retype(traversal.et, traversal.et),
                  step.until.map(_.retype(step.traversal.et, step.traversal.et)),
                  step.max,
                  step.collect,
                  step.noloop)
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversal.et)
              case step: Union[_, _] =>
                val typedStep = Union(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversals.map(_.et).reduce(_ + _))
            }
        }
        retype(steps, typedTraversal)
      case (step: ProjectionStep) +: steps =>
        val typedTraversal = step match {
          case step: MapStep =>
            new Traversal(step :: traversal.steps)(traversal.st,
              MapType(DataType.default.`@property`, ListType(ClassType.stubAny)))
          case step: Select[_] =>
            val labels = traversal.stepsList.collect { case step: As[_, _] => step }
            val types  = step.names.map(name => labels.find(_.label == name).map(_.ct))
            new Traversal(step :: traversal.steps)(traversal.st, TupleType(types))
          case step: Project[_] =>
            val typedStep = Project(
              step.by.runtimeList.reverse
                .map {
                  case t: Traversal[_, _, _] =>
                    t.retype(traversal.et, traversal.et)
                }
                .foldLeft[HList](HNil) { case (r, t) => t :: r })
            val typedProjections = typedStep.by.runtimeList
            val et = typedProjections.lengthCompare(1) match {
              case -1 => traversal.et
              case 0  => typedProjections.headOption.map { case t: Traversal[_, _, _] => t.enclosedEndType }.get
              case 1 =>
                TupleType(
                  typedProjections.reverse
                    .map { case t: Traversal[_, _, _] => t.enclosedEndType }
                    .map(Some(_)))
            }
            new Traversal(typedStep :: traversal.steps)(traversal.st, et)
          case step: Path[_, _] =>
            val typedStep = Path(step.by.retype(traversal.et, traversal.et))
            new Traversal(typedStep :: traversal.steps)(traversal.st, ListType[Any](typedStep.by.et))
        }
        retype(steps, typedTraversal)
      case (step: ReducingStep) +: steps =>
        val typedTraversal = step match {
          case step: ReducingBarrierStep =>
            step match {
              case step: Mean =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et) //int to double?
              case step: Sum =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case _ => throw new Exception("unexpected")
            }
          case step: Head =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Last =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Min =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Max =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
        }
        retype(steps, typedTraversal)
      case (step: FilterStep) +: steps =>
        val typedTraversal = step match {
          case step: And =>
            val typedStep = And(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: Coin =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Is =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Not =>
            val typedStep = Not(step.traversal.retype(traversal.et, traversal.et))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: Or =>
            val typedStep = Or(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: Where =>
            val typedStep = Where(step.traversal.retype(traversal.et, traversal.et))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: GlobalFilterStep =>
            step match {
              case step: Dedup =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
            }
          case step: ClipStep =>
            step match {
              case step: Limit =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case step: Range =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case step: Tail =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case step: Skip =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
            }
          case step: HasStep =>
            step match {
              case step: HasLabel =>
                new Traversal(step :: traversal.steps)(traversal.st,
                  step.label.reduceOption(_ + _).getOrElse(ClassType.stubNothing))
              case _ => new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
            }
        }
        retype(steps, typedTraversal)
      case (step: BarrierStep) +: steps =>
        val typedTraversal = step match {
          case step: Count =>
            new Traversal(step :: traversal.steps)(traversal.st, `@long`)
          case step: GroupingBarrierStep =>
            step match {
              case step: Group[_, _, _, _] =>
                val typedStep = Group(
                  step.by.retype(traversal.et, traversal.et),
                  step.value.retype(traversal.et, traversal.et)
                )
                new Traversal(typedStep :: traversal.steps)(
                  traversal.st,
                  TupleType(
                    List(
                      Some(typedStep.by.enclosedEndType),
                      Some((lspace.g
                        .out()
                        .untyped /*hack/manipulation because mapValues operates on a collection of traversers and typing assumes a singlepoint origin*/ ++ typedStep.value.untyped).toTyped
                        .retype(traversal.et, traversal.et)
                        .enclosedEndType)
                    ))
                )
            }
          case step: Order =>
            val typedStep = Order(step.by.retype(traversal.et, traversal.et), step.increasing)
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
        }
        retype(steps, typedTraversal)
      case (step: As[_, _]) +: steps =>
        val typedTraversal = new Traversal(As(step.label)(traversal.et) :: traversal.steps)(traversal.st, traversal.et)
        retype(steps, typedTraversal)
      //      case _             =>
    }
}
