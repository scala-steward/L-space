package lspace.librarian.traversal.util

import lspace.datatype._
import lspace.librarian.traversal.step._
import lspace.librarian.traversal._
import lspace.structure.ClassType
import shapeless.{::, <:!<, =:!=, HList, HNil, Poly1}

sealed trait EndMapper[ET <: ClassType[Any], Steps <: HList] {
  type Out
  type OutCT <: ClassType[Any]

  def map(et: ET): OutCT
}

object EndMapper {
  type Aux[ET <: ClassType[Any], Steps <: HList, Out0, OutCT0 <: ClassType[Any]] = EndMapper[ET, Steps] {
    type Out   = Out0
    type OutCT = OutCT0
  }

  implicit def outtweaker[ET <: ClassType[Any], Steps <: HList, Out0, OutCT0 <: ClassType[Any]](
      implicit tweaker0: EndMapper0[ET, Steps, Out0, OutCT0]): Aux[ET, Steps, Out0, OutCT0] =
    new EndMapper[ET, Steps] {
      type Out   = Out0
      type OutCT = OutCT0

      def map(et: ET): OutCT = tweaker0.map(et)
    }

  trait EndMapper0[ET <: ClassType[Any], Steps <: HList, Out, OutCT <: ClassType[Any]] {
    def map(et: ET): OutCT
  }

  trait LowPriorityOutTweaker0 {
    implicit def norelevantsteps[End, ET[+Z] <: ClassType[Z], Steps <: HList, Out0](
        implicit ev: Steps =:!= HNil): EndMapper0[ET[End], Steps, List[End], ListType[List[End]]] =
      new EndMapper0[ET[End], Steps, List[End], ListType[List[End]]] {
        def map(et: ET[End]): ListType[List[End]] =
          ListType(et)
      }
  }

  object EndMapper0 extends LowPriorityOutTweaker0 {

    object ReducedEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: ReducingStep => false
          case _                  => true
        } match {
          case (Nil, Nil)   => false //not reduced
          case (steps, Nil) => false //not reduced
//          case (steps, reduceStep :: leadSteps) if !BranchedEnd.is(steps) && !ResourcedEnd.is(steps) => true
          case (steps, reduceStep :: leadSteps)
              if steps.forall(ReducedEndInvariant.isInvariant) && BranchedOrResourcedEnd.is(leadSteps) => //perhaps just look for at least one branche or resourche step
            true
          case _ => false
        }
      implicit def reduced[T <: ReducingStep]: Case.Aux[T, T] = at[T](identity)
    }
    object ReducedEndInvariant extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: FilterStep      => true
        case step: EnvironmentStep => true
        case step: TraverseStep    => true
        case step: ProjectionStep  => true
        case _                     => false
      }
      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
      implicit def constant[T <: TraverseStep]: Case.Aux[T, T]       = at[T](identity)
      implicit def projection[T <: ProjectionStep]: Case.Aux[T, T]   = at[T](identity)
    }

    implicit def reducedEnd[End,
                            ET[+Z] <: ClassType[Z],
                            Steps <: HList,
                            Filters <: HList,
                            Tail <: HList,
                            BranchedOrResourced](
        implicit
        splitter: CoSplitLeft.Aux[Steps, ReducingStep, Filters, Tail],
        collect: shapeless.ops.hlist.Collect.Aux[Filters, ReducedEndInvariant.type, Filters],
        tailIsBrancedOrResourced: shapeless.ops.hlist.CollectFirst.Aux[Tail,
                                                                       BranchedOrResourcedEnd.type,
                                                                       BranchedOrResourced]
        //tail contains branch step?
    ): EndMapper0[ET[End], Steps, Option[End], OptionType[Option[End]]] =
      new EndMapper0[ET[End], Steps, Option[End], OptionType[Option[End]]] {
        def map(et: ET[End]): OptionType[Option[End]] = OptionType(et)
      }

    object SingularEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: Count => false
          case _           => true
        } match {
          case (steps, Nil)                                                                         => false //not singular
          case (steps, singularStep :: leadSteps) if steps.forall(SingularEndInvariant.isInvariant) => true
          case _                                                                                    => false
        }
      implicit def counted[T <: Count]: Case.Aux[T, T] = at[T](identity)
    }

    object SingularEndInvariant extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: Constant[_]    => true
        case step: ProjectionStep => true
        case _                    => false
      }
      implicit def constant[T <: Constant[_]]: Case.Aux[T, T]     = at[T](identity)
      implicit def projected[T <: ProjectionStep]: Case.Aux[T, T] = at[T](identity)
    }

    //Q: IDEA does not find implicit when ClassType[End] is changed to ET[End], compiling is no problem
    implicit def singularEnd[End,
                             ET[Z] <: ClassType[Z],
                             Steps <: HList,
                             Filters <: HList,
                             Tail <: HList,
                             Filters1 <: HList](
        implicit
        splitter: CoSplitLeft.Aux[Steps, Count, Filters, Tail],
        collect: shapeless.ops.hlist.Collect.Aux[Filters, SingularEndInvariant.type, Filters1],
        noF1: Filters1 =:= Filters): EndMapper0[ET[End], Steps, End, ClassType[End]] =
      new EndMapper0[ET[End], Steps, End, ClassType[End]] {

        def map(et: ET[End]): ClassType[End] = et
      }

    object FilteredEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: FilterStep      => true
          case step: EnvironmentStep => true
//          case step: ReducingStep    => false
          case _ => false
        } match {
          case (Nil, Nil)           => false //no filtered end
          case (step :: steps, Nil) => true
          case (Nil, step :: steps) => false
          case (steps, leadSteps)
//              if steps.forall(FilteredEndInvariant.isInvariant) && !BranchedOrResourcedEnd.is(leadSteps) =>
              if !BranchedOrResourcedEnd.is(leadSteps) =>
            true
          case _ => false
        }

      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
    }
    object FilteredEndInvariant extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: TraverseStep    => true
        case step: ProjectionStep  => true
        case step: EnvironmentStep => true
        case _                     => false
      }
      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
    }

    implicit def filteredEnd[End,
                             ET[+Z] <: ClassType[Z],
                             Step,
                             Steps <: HList,
                             Prefix <: HList,
                             Suffix <: HList,
                             BranchedOrResourced <: HList](
        implicit
        collect: Span.Aux[Step :: Steps, FilteredEnd.type, Prefix, Suffix],
        tailIsBrancedOrResourced: shapeless.ops.hlist.Collect.Aux[Suffix,
                                                                  BranchedOrResourcedEnd.type,
                                                                  BranchedOrResourced],
        ev: BranchedOrResourced =:= HNil): EndMapper0[ET[End], Step :: Steps, Option[End], OptionType[Option[End]]] =
      new EndMapper0[ET[End], Step :: Steps, Option[End], OptionType[Option[End]]] {

        def map(et: ET[End]): OptionType[Option[End]] = OptionType(et)
      }

//    object SingularFilteredEnd extends Poly1 {
//      import scala.collection.immutable.::
//      def is(steps: List[Step]): Boolean =
//        steps.span {
//          case step: Count => false
//          case _           => true
//        } match {
//          case (steps, Nil) => false //not singular
//          case (steps, singularStep :: leadSteps)
//              if steps.forall(SingularFilteredEndInvariant.isInvariant) && steps.exists {
//                case step: FilterStep      => true
//                case step: EnvironmentStep => true
//                case _                     => false
//              } =>
//            true
//        }
//    }
//
//    object SingularFilteredEndInvariant extends Poly1 {
//      def isInvariant(step: Step): Boolean = step match {
//        case step: Constant[_]     => true
//        case step: FilterStep      => true
//        case step: ProjectionStep  => true
//        case step: EnvironmentStep => true
//        case _                     => false
//      }
//      implicit def constant[T <: Constant[_]]: Case.Aux[T, T]        = at[T](identity)
//      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
//      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
//    }
//
//    implicit def filteredSingularEnd[End,
//                                     ET[+Z] <: ClassType[Z],
//                                     Steps <: HList,
//                                     Steps0 <: HList,
//                                     Steps1 <: HList,
//                                     Filters <: HList,
//                                     Tail <: HList](
//        implicit
//        splitter: CoSplitLeft.Aux[Steps, Count, Steps0, Tail],
//        ev: Steps0 =:!= HNil,
//        collect: shapeless.ops.hlist.Collect.Aux[Steps0, FilteredEnd.type, Steps1],
//        steps1: Steps1 =:!= HNil,
//        collect1: shapeless.ops.hlist.Collect.Aux[Steps0, SingularFilteredEndInvariant.type, Steps0])
//      : EndMapper0[ET[End], Steps, Option[End], OptionType[Option[End]]] =
//      new EndMapper0[ET[End], Steps, Option[End], OptionType[Option[End]]] {
//
//        def map(et: ET[End]): OptionType[Option[End]] = OptionType(et)
//      }

    implicit def nosteps[End, ET[+Z] <: ClassType[Z], Out0]: EndMapper0[ET[End], HNil, End, ET[End]] =
      new EndMapper0[ET[End], HNil, End, ET[End]] {

        def map(et: ET[End]): ET[End] =
          et
      }

    object BranchedEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: BranchStep => false
          case _                => true
        } match {
          case (Nil, Nil)           => false //not a branched end
          case (step :: steps, Nil) => false //not a branched end
          case (steps, branchStep :: leadSteps) if steps.forall(BranchedEndCompatible.isInvariant) =>
            true
          case _ => false
        }
      implicit def branched[T <: BranchStep]: Case.Aux[T, T] = at[T](identity)
    }
    object BranchedEndCompatible extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: FilterStep      => true
        case step: EnvironmentStep => true
        case step: TraverseStep    => true
        case step: ProjectionStep  => true
//        case step: GlobalFilterStep => true
        case _ => false
      }
      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
      implicit def traverse[T <: TraverseStep]: Case.Aux[T, T]       = at[T](identity)
      implicit def projection[T <: ProjectionStep]: Case.Aux[T, T]   = at[T](identity)
//      implicit def globalFilter[T <: GlobalFilterStep]: Case.Aux[T, T] = at[T](identity)
    }

    object BranchedEndIncompatible extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: ReducingStep => true
        case step: Dedup        => true
        case _                  => false
      }
      implicit def reduce[T <: ReducingStep]: Case.Aux[T, T] = at[T](identity)
      implicit def dedup[T <: Dedup]: Case.Aux[T, T]         = at[T](identity)
      //    implicit def globalFilterStep[T <: GlobalFilterStep] = at[T](identity)
    }

//    implicit def branchedEnd[End,
//                             ET[+Z] <: ClassType[Z],
//                             Steps <: HList,
//                             Filters <: HList,
//                             Tail <: HList,
//                             InvalidFilters <: HList](
//        implicit
//        filterSplitter: CoSplitLeft.Aux[Steps, BranchStep, Filters, Tail],
//        collectValidFilters: shapeless.ops.hlist.Collect.Aux[Filters, BranchedEndCompatible.type, Filters], //WORKS? 2xFilters
//        collectInvalidFilters: shapeless.ops.hlist.Collect.Aux[Filters, BranchedEndIncompatible.type, InvalidFilters],
//        noinvalid: InvalidFilters =:= HNil
//    ): EndMapper0[ET[End], Steps, List[End], ListType[List[End]]] =
//      new EndMapper0[ET[End], Steps, List[End], ListType[List[End]]] {
//
//        def map(et: ET[End]): ListType[List[End]] =
//          ListType(et)
//      }

    object ResourcedEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: ResourceStep => false
          case _                  => true
        } match {
          case (steps, Nil)                                                                           => false //not a resourced end
          case (steps, singularStep :: leadSteps) if steps.forall(ResourcedEndCompatible.isInvariant) => true
          case _                                                                                      => false
        }
      implicit def resource[T <: ResourceStep]: Case.Aux[T, T] = at[T](identity)
    }
    object ResourcedEndCompatible extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: FilterStep      => true
        case step: EnvironmentStep => true
        case step: TraverseStep    => true
        case step: ProjectionStep  => true
        case _                     => false
      }
      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
      implicit def constant[T <: TraverseStep]: Case.Aux[T, T]       = at[T](identity)
      implicit def projection[T <: ProjectionStep]: Case.Aux[T, T]   = at[T](identity)
    }

    object ResourcedEndIncompatible extends Poly1 {
      implicit def move[T <: MoveStep]: Case.Aux[T, T]       = at[T](identity)
      implicit def count[T <: Count]: Case.Aux[T, T]         = at[T](identity)
      implicit def reduce[T <: ReducingStep]: Case.Aux[T, T] = at[T](identity)
      implicit def dedup[T <: Dedup]: Case.Aux[T, T]         = at[T](identity)

      //    implicit def globalFilterStep[T <: GlobalFilterStep] = at[T](identity)
    }

//    implicit def resourcedEnd[End,
//                              ET[+Z] <: ClassType[Z],
//                              Steps <: HList,
//                              Filters <: HList,
//                              Tail <: HList,
//                              InvalidFilters <: HList](
//        implicit
//        filterSplitter: CoSplitLeft.Aux[Steps, ResourceStep, Filters, Tail],
//        collectValidFilters: shapeless.ops.hlist.Collect.Aux[Filters, ResourcedEndCompatible.type, Filters],
//        collectInvalidFilters: shapeless.ops.hlist.Collect.Aux[Filters, ResourcedEndIncompatible.type, InvalidFilters],
//        noinvalid: InvalidFilters =:= HNil
//    ): EndMapper0[ET[End], Steps, List[End], ListType[List[End]]] =
//      new EndMapper0[ET[End], Steps, List[End], ListType[List[End]]] {
//
//        def map(et: ET[End]): ListType[List[End]] =
//          ListType(et)
//      }

    object DistinctedEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: Dedup => false
          case _           => true
        } match {
          case (steps, Nil) => false //not a distincted end
          case (steps, singularStep :: leadSteps)
              if steps.forall(DistinctedEndInvariant.isInvariant) && BranchedOrResourcedEnd.is(
                singularStep :: leadSteps) =>
            true
        }
      implicit def dedup[T <: Dedup]: Case.Aux[T, T] = at[T](identity)
    }
    object DistinctedEndInvariant extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: FilterStep      => true
        case step: Id              => true
        case step: EnvironmentStep => true
        case _                     => false
      }
      implicit def id[T <: Id]: Case.Aux[T, T]                       = at[T](identity)
      implicit def filters[T <: FilterStep]: Case.Aux[T, T]          = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
    }

    object BranchedOrResourcedEnd extends Poly1 {
      def is(steps: List[Step]): Boolean                       = BranchedEnd.is(steps) || ResourcedEnd.is(steps)
      implicit def branched[T <: BranchStep]: Case.Aux[T, T]   = at[T](identity)
      implicit def resource[T <: ResourceStep]: Case.Aux[T, T] = at[T](identity)
    }

    implicit def distinctedEnd[End,
                               ET[+Z] <: ClassType[Z],
                               Steps <: HList,
                               Filters <: HList,
                               Tail <: HList,
                               BranchedOrResourced](
        implicit
        splitter: CoSplitLeft.Aux[Steps, Dedup, Filters, Tail],
        collect: shapeless.ops.hlist.Collect.Aux[Filters, DistinctedEndInvariant.type, Filters],
        collectBranchedOrResourced: shapeless.ops.hlist.CollectFirst.Aux[Tail,
                                                                         BranchedOrResourcedEnd.type,
                                                                         BranchedOrResourced])
      : EndMapper0[ET[End], Steps, Set[End], SetType[Set[End]]] =
      new EndMapper0[ET[End], Steps, Set[End], SetType[Set[End]]] {

        def map(et: ET[End]): SetType[Set[End]] = SetType(et)
      }

    object GroupedEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: GroupingBarrierStep => false
          case _                         => true
        } match {
          case (steps, Nil)                                                                        => false //not a grouped end
          case (steps, singularStep :: leadSteps) if steps.forall(GroupedEndInvariant.isInvariant) => true
          case _                                                                                   => false
        }
      implicit def grouped[T <: GroupingBarrierStep]: Case.Aux[T, T] = at[T](identity)
    }

    object GroupedEndInvariant extends Poly1 {
      def isInvariant(step: Step): Boolean = step match {
        case step: FilterStep      => true
        case step: EnvironmentStep => true
        case _                     => false
      }
      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
    }

    implicit def groupedEnd[K, V, Steps <: HList, Filters <: HList, Filters2 <: HList, Tail <: HList](
        implicit
        splitter: CoSplitLeft.Aux[Steps, GroupingBarrierStep, Filters, Tail],
        collect: shapeless.ops.hlist.Collect.Aux[Filters, GroupedEndInvariant.type, Filters2],
        ev: Filters =:= Filters2): EndMapper0[TupleType[(K, V)], Steps, Map[K, V], MapType[Map[K, V]]] =
      new EndMapper0[TupleType[(K, V)], Steps, Map[K, V], MapType[Map[K, V]]] {

        def map(et: TupleType[(K, V)]): MapType[Map[K, V]] =
          //        ListType(
          MapType(
            et.rangeTypes.head
              .asInstanceOf[Option[ClassType[K]]]
              .getOrElse(ClassType.stubAny.asInstanceOf[ClassType[K]]),
            et.rangeTypes.tail.head
              .asInstanceOf[Option[ClassType[V]]]
              .getOrElse(ClassType.stubAny.asInstanceOf[ClassType[V]])
          ) //)
      }

    object OneOnOneEnd extends Poly1 {
      import scala.collection.immutable.::
      def is(steps: List[Step]): Boolean =
        steps.span {
          case step: TraverseStep   => true
          case step: ProjectionStep => true
          case _                    => false
        } match {
          case (step :: steps, Nil) => true //not a projected end
//          case (steps, singularStep :: leadSteps)
//              if (singularStep :: leadSteps).forall(OneOnOneEndInvariant.isInvariant) =>
//            true
          case _ => false
        }
      implicit def projected[T <: ProjectionStep]: Case.Aux[T, T] = at[T](identity)
    }

//    object OneOnOneEndInvariant extends Poly1 {
//      def isInvariant(step: Step): Boolean = step match {
//        case step: TraverseStep => true
//        case _                  => false
//      }
//      implicit def filter[T <: FilterStep]: Case.Aux[T, T]           = at[T](identity)
//      implicit def environment[T <: EnvironmentStep]: Case.Aux[T, T] = at[T](identity)
//    }

    implicit def oneOnOneEnd[End, ET[+Z] <: ClassType[Z], Steps <: HList, P <: HList, S <: HList, FS <: HList](
        implicit
        filterSplitter: Span.Aux[Steps, OneOnOneEnd.type, P, S],
        atLeastOne: P =:!= HNil,
//        collect: shapeless.ops.hlist.Collect.Aux[S, OneOnOneEndInvariant.type, FS],
        noTail: S =:= HNil
    ): EndMapper0[ET[End], Steps, End, ClassType[End]] =
      new EndMapper0[ET[End], Steps, End, ClassType[End]] {

        def map(et: ET[End]): ClassType[End] = et
      }
  }

  /**
    * @param traversal
    * @return
    */
  def tweakEnd(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], HList]): ClassType[Any] = {
    //FilterBarrierStep
    //ReducingBarrierStep
    import scala.collection.immutable.::
    traversal.stepsList.reverse.span {
      case _: ReducingStep | _: Dedup                                                   => false
      case _: FilterStep | _: EnvironmentStep | _: Project[_] | _: Id | _: To | _: From => true
      case _                                                                            => false
    } match {
      case (toIgnore, (_: ReducingStep) :: steps) =>
        steps.span {
          case _: ReducingStep | _: FilterStep | _: EnvironmentStep => true
          case _                                                    => false
        } match {
          case (toIgnore2, List(step: Group[_, _, _, _], _*)) =>
            OptionType(MapType(step.by.et, step.value.et))
//            OptionType(TupleType(List(Some(step.by.et), Some(step.value.et)))) //OptionType(traversal.et)
          case _ => OptionType(traversal.et)
        }
      case (List(), List(step: Group[_, _, _, _], _*)) =>
        MapType(step.by.et, step.value.et)
      case (toIgnore, List(step: Dedup, _*)) =>
        SetType(traversal.et)
      case (toIgnore, (step: Count) :: steps) if toIgnore.nonEmpty && toIgnore.exists(_.isInstanceOf[FilterStep]) =>
        OptionType(traversal.et)
      case (List(), List(step: Count, _*)) =>
        traversal.et
      case _ => ListType(traversal.et)
    }
  }

}
