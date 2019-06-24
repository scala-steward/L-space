package lspace.librarian.traversal

import lspace.datatype.{
  CollectionType,
  ListSetType,
  ListType,
  LongType,
  MapType,
  OptionType,
  SetType,
  TupleType,
  VectorType
}
import lspace.Label.D._
import lspace.librarian.traversal.step.{
  Constant,
  Count,
  Dedup,
  From,
  Group,
  Head,
  Id,
  Is,
  Last,
  Local,
  Max,
  Mean,
  Min,
  Path,
  Project,
  Sum,
  To
}
import lspace.structure.ClassType
import shapeless.{::, <:!<, =:!=, HList, HNil, Poly, Poly1, Poly2}
import shapeless.ops.hlist._

import scala.collection.immutable.ListSet

sealed trait OutTweaker[ET <: ClassType[Any], Steps <: HList] {
  type Out
  type OutCT <: ClassType[_]

  def tweak(et: ET): OutCT
}

object OutTweaker {
  type Aux[ET <: ClassType[Any], Steps <: HList, Out0, OutCT0 <: ClassType[Any]] = OutTweaker[ET, Steps] {
    type Out   = Out0
    type OutCT = OutCT0
  }

  object OptionEndInvariant extends Poly1 {
    implicit def filter[T <: FilterStep]           = at[T](identity)
    implicit def environment[T <: EnvironmentStep] = at[T](identity)
    implicit def constant[T <: TraverseStep]       = at[T](identity)
    implicit def projection[T <: ProjectionStep]   = at[T](identity)
  }
  implicit def optionEnd[End, ET[+Z] <: ClassType[Z], Steps <: HList, Filters <: HList, Tail <: HList](
      implicit
      splitter: CoSplitLeft.Aux[Steps, ReducingStep, Filters, Tail],
      collect: shapeless.ops.hlist.Collect.Aux[Filters, OptionEndInvariant.type, Filters]
      //tail contains branch step?
  ): OutTweaker.Aux[ET[End], Steps, Option[End], OptionType[Option[End]]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = Option[End]
      type OutCT = OptionType[Option[End]]
      def tweak(et: ET[End]): OptionType[Option[End]] = OptionType(et)
    }

  object EndInvariant extends Poly1 {
    implicit def constant[T <: Constant[_]] = at[T](identity)
  }

  //Q: IDEA does not find implicit when ClassType[End] is changed to ET[End], compiling is no problem
  implicit def countEnd[End, ET[Z] <: ClassType[Z], Steps <: HList, Filters <: HList, Tail <: HList](
      implicit
      splitter: CoSplitLeft.Aux[Steps, Count, Filters, Tail],
      collect: shapeless.ops.hlist.Collect.Aux[Filters, EndInvariant.type, Filters])
    : OutTweaker.Aux[ET[End], Steps, End, ClassType[End]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = End
      type OutCT = ClassType[End]
      def tweak(et: ET[End]): ClassType[End] = et
    }

  trait filterSteps extends Poly1 {
    implicit def filter[T <: FilterStep]           = at[T](identity)
    implicit def environment[T <: EnvironmentStep] = at[T](identity)
  }
  object FilterOnly extends filterSteps

  implicit def filterEnd[End, ET[+Z] <: ClassType[Z], Step, Steps <: HList, Prefix <: HList, Suffix <: HList](
      implicit
      collect: Span.Aux[Step :: Steps, FilterOnly.type, Prefix, Suffix],
      ev: Suffix =:= HNil): OutTweaker.Aux[ET[End], Step :: Steps, Option[End], OptionType[Option[End]]] =
    new OutTweaker[ET[End], Step :: Steps] {
      type Out   = Option[End]
      type OutCT = OptionType[Option[End]]
      def tweak(et: ET[End]): OptionType[Option[End]] = OptionType(et)
    }

  object CountOptionEndInvariant extends Poly1 {
    implicit def filter[T <: FilterStep]           = at[T](identity)
    implicit def environment[T <: EnvironmentStep] = at[T](identity)
  }
  implicit def countOptionEnd[End,
                              ET[+Z] <: ClassType[Z],
                              Steps <: HList,
                              Steps0 <: HList,
                              Filters <: HList,
                              Tail <: HList](
      implicit
      splitter: CoSplitLeft.Aux[Steps, Count, Steps0, Tail],
      ev: Steps0 =:!= HNil,
      collect: shapeless.ops.hlist.Collect.Aux[Steps0, CountOptionEndInvariant.type, Steps0])
    : OutTweaker.Aux[ET[End], Steps, Option[End], OptionType[Option[End]]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = Option[End]
      type OutCT = OptionType[Option[End]]
      def tweak(et: ET[End]): OptionType[Option[End]] = OptionType(et)
    }

  implicit def nosteps[End, ET[+Z] <: ClassType[Z], Out0]
    : OutTweaker.Aux[ET[End], HNil, List[End], ListType[List[End]]] =
    new OutTweaker[ET[End], HNil] {
      type Out   = List[End]
      type OutCT = ListType[List[End]]
      def tweak(et: ET[End]): ListType[List[End]] =
        ListType(et)
    }
//  implicit def nosteps2[K, V, Out0]: OutTweaker.Aux[MapType[K, V], HNil, List[Map[K, V]], ListType[Map[K, V]]] =
//    new OutTweaker[MapType[K, V], HNil] {
//      type Out   = List[Map[K, V]]
//      type OutCT = ListType[Map[K, V]]
//      def tweak(et: MapType[K, V]): ListType[Map[K, V]] =
//        ListType(et)
//    }
//  implicit def nosteps3[End, Out0]: OutTweaker.Aux[ListType[End], HNil, List[List[End]], ListType[List[End]]] =
//    new OutTweaker[ListType[End], HNil] {
//      type Out   = List[List[End]]
//      type OutCT = ListType[List[End]]
//      def tweak(et: ListType[End]): ListType[List[End]] =
//        ListType(et)
//    }

  object ListCompatible extends Poly1 {
    implicit def filter[T <: FilterStep]             = at[T](identity)
    implicit def environment[T <: EnvironmentStep]   = at[T](identity)
    implicit def constant[T <: TraverseStep]         = at[T](identity)
    implicit def projection[T <: ProjectionStep]     = at[T](identity)
    implicit def globalFilter[T <: GlobalFilterStep] = at[T](identity)
  }
  object ListIncompatible extends Poly1 {
    implicit def reduce[T <: ReducingStep] = at[T](identity)
    implicit def dedup[T <: Dedup]         = at[T](identity)
//    implicit def globalFilterStep[T <: GlobalFilterStep] = at[T](identity)
  }
  object BranchStepCompatible extends Poly1 {
    implicit def filter[T <: FilterStep]             = at[T](identity)
    implicit def environment[T <: EnvironmentStep]   = at[T](identity)
    implicit def constant[T <: TraverseStep]         = at[T](identity)
    implicit def projection[T <: ProjectionStep]     = at[T](identity)
    implicit def globalFilter[T <: GlobalFilterStep] = at[T](identity)
  }
  object BranchStepIncompatible extends Poly1 {
    implicit def reduce[T <: ReducingStep] = at[T](identity)
    implicit def dedup[T <: Dedup]         = at[T](identity)
    //    implicit def globalFilterStep[T <: GlobalFilterStep] = at[T](identity)
  }

  implicit def branchStep[End,
                          ET[+Z] <: ClassType[Z],
                          Steps <: HList,
                          Filters <: HList,
                          Tail <: HList,
                          InvalidFilters <: HList](
      implicit
      filterSplitter: CoSplitLeft.Aux[Steps, BranchStep, Filters, Tail],
      collectValidFilters: shapeless.ops.hlist.Collect.Aux[Filters, BranchStepCompatible.type, Filters],
      collectInvalidFilters: shapeless.ops.hlist.Collect.Aux[Filters, BranchStepIncompatible.type, InvalidFilters],
      noinvalid: InvalidFilters =:= HNil
  ): Aux[ET[End], Steps, List[End], ListType[List[End]]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = List[End]
      type OutCT = ListType[List[End]]
      def tweak(et: ET[End]): ListType[List[End]] =
        ListType(et)
    }

  object ResourceCompatible extends Poly1 {
    implicit def filter[T <: FilterStep]           = at[T](identity)
    implicit def environment[T <: EnvironmentStep] = at[T](identity)
    implicit def constant[T <: TraverseStep]       = at[T](identity)
    implicit def projection[T <: ProjectionStep]   = at[T](identity)
  }
  object ResourceStepIncompatible extends Poly1 {
    implicit def move[T <: MoveStep]       = at[T](identity)
    implicit def count[T <: Count]         = at[T](identity)
    implicit def reduce[T <: ReducingStep] = at[T](identity)
    implicit def dedup[T <: Dedup]         = at[T](identity)
    //    implicit def globalFilterStep[T <: GlobalFilterStep] = at[T](identity)
  }
  implicit def resourceStep[End,
                            ET[+Z] <: ClassType[Z],
                            Steps <: HList,
                            Filters <: HList,
                            Tail <: HList,
                            InvalidFilters <: HList](
      implicit
      filterSplitter: CoSplitLeft.Aux[Steps, ResourceStep, Filters, Tail],
      collectValidFilters: shapeless.ops.hlist.Collect.Aux[Filters, ResourceCompatible.type, Filters],
      collectInvalidFilters: shapeless.ops.hlist.Collect.Aux[Filters, ResourceStepIncompatible.type, InvalidFilters],
      noinvalid: InvalidFilters =:= HNil
  ): Aux[ET[End], Steps, List[End], ListType[List[End]]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = List[End]
      type OutCT = ListType[List[End]]
      def tweak(et: ET[End]): ListType[List[End]] =
        ListType(et)
    }

  object FilterDedupCompatible extends Poly1 {
    implicit def filters[T <: FilterStep]          = at[T](identity)
    implicit def environment[T <: EnvironmentStep] = at[T](identity)
  }
  implicit def filtersDedupEnd[End, ET[+Z] <: ClassType[Z], Steps <: HList, Filters <: HList, Tail <: HList](
      implicit
      splitter: CoSplitLeft.Aux[Steps, Dedup, Filters, Tail],
      collect: shapeless.ops.hlist.Collect.Aux[Filters, FilterDedupCompatible.type, Filters])
    : Aux[ET[End], Steps, Set[End], SetType[Set[End]]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = Set[End]
      type OutCT = SetType[Set[End]]
      def tweak(et: ET[End]): SetType[Set[End]] = SetType(et)
    }

  object MapEndInvariant extends Poly1 {
    implicit def filter[T <: FilterStep]           = at[T](identity)
    implicit def environment[T <: EnvironmentStep] = at[T](identity)
  }

  implicit def mapEnd[K, V, Steps <: HList, Filters <: HList, Tail <: HList](
      implicit
      splitter: CoSplitLeft.Aux[Steps, GroupingBarrierStep, Filters, Tail],
      collect: shapeless.ops.hlist.Collect.Aux[Filters, MapEndInvariant.type, Filters])
    : Aux[TupleType[(K, V)], Steps, Map[K, V], MapType[Map[K, V]]] =
    new OutTweaker[TupleType[(K, V)], Steps] {
      type Out   = Map[K, V] //remove List?
      type OutCT = MapType[Map[K, V]]
      def tweak(et: TupleType[(K, V)]): MapType[Map[K, V]] =
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

  object ProjectionLike extends Poly1 {
    implicit def projection[T <: ProjectionStep] = at[T](identity)
  }
  implicit def projectionStep[End, ET[+Z] <: ClassType[Z], Steps <: HList, P <: HList, S <: HList](
      implicit
      filterSplitter: Span.Aux[Steps, ProjectionLike.type, P, S],
      atLeastOne: P =:!= HNil,
      noTail: S =:= HNil
  ): Aux[ET[End], Steps, End, ClassType[End]] =
    new OutTweaker[ET[End], Steps] {
      type Out   = End
      type OutCT = ClassType[End]
      def tweak(et: ET[End]): ClassType[End] = et
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
