package lspace.librarian.traversal

import lspace.datatype.{CollectionType, ListType, MapType, OptionType, TupleType}
import lspace.Label.D._
import lspace.librarian.traversal.step.{Count, Group, Head, Is, Last, Max, Mean, Min, Project, Sum}
import lspace.structure.ClassType
import shapeless.{::, <:!<, HList, HNil, Poly2}

sealed trait OutTweaker[ET <: ClassType[_], Containers <: HList] {
  type Out
  type OutCT <: ClassType[_]

  def tweak(et: ET): OutCT
}

object OutTweaker {
  type Aux[ET <: ClassType[_], Containers <: HList, Out0, OutCT0 <: ClassType[_]] = OutTweaker[ET, Containers] {
    type Out   = Out0
    type OutCT = OutCT0
  }
  implicit def nocontainers[End, ET[+Z] <: ClassType[Z]]
    : OutTweaker.Aux[ET[End], HNil, List[End], ClassType[List[End]]] = new OutTweaker[ET[End], HNil] {
    type Out   = List[End]
    type OutCT = ClassType[List[End]]
    def tweak(et: ET[End]): ClassType[List[End]] =
      ListType(et :: Nil filter (_.iri.nonEmpty)) //.asInstanceOf[ClassType[List[End]]]
  }
  //  implicit def containers[ET, SET, Container, Containers <: HList] = new OutTweaker[ET, SET, Container :: Containers] {
  //    type Out = SET
  //    def tweak[CT <: ClassType[SET]](ct1: ET, ct2: CT): ClassType[Out] = ct2
  //  }
  implicit def containersMap[K, V, Container, Containers <: HList](implicit ev: Container <:< Group[_, _, _, _])
    : OutTweaker.Aux[TupleType[(K, V)], Container :: Containers, List[Map[K, V]], ClassType[List[Map[K, V]]]] =
    new OutTweaker[TupleType[(K, V)], Container :: Containers] {
      type Out   = List[Map[K, V]]
      type OutCT = ClassType[List[Map[K, V]]]
      def tweak(et: TupleType[(K, V)]): ClassType[List[Map[K, V]]] =
        ListType(
          MapType[K, V](
            et.rangeTypes.head.asInstanceOf[List[ClassType[K]]] filter (_.iri.nonEmpty),
            et.rangeTypes.tail.head
              .asInstanceOf[List[ClassType[V]]] filter (_.iri.nonEmpty)
          ) :: Nil)
    }
  sealed trait IsListEnd[T]
  object IsListEnd {
    implicit def project[T <: HList]   = new IsListEnd[Project[T]] {}
    implicit def mapstep[T <: MapStep] = new IsListEnd[T]          {}
  }
  implicit def containersList[End, ET[+Z] <: ClassType[Z], Container, Containers <: HList](
      implicit ev: IsListEnd[Container])
    : OutTweaker.Aux[ET[End], Container :: Containers, List[End], ClassType[List[End]]] =
    new OutTweaker[ET[End], Container :: Containers] {
      type Out   = List[End]
      type OutCT = ClassType[List[End]]
      def tweak(et: ET[End]): ClassType[List[End]] = ListType(et :: Nil filter (_.iri.nonEmpty))
    }
  implicit def containersList2[K, V, Container, Containers <: HList](implicit ev: IsListEnd[Container])
    : OutTweaker.Aux[MapType[K, V], Container :: Containers, Map[K, V], ClassType[List[Map[K, V]]]] =
    new OutTweaker[MapType[K, V], Container :: Containers] {
      type Out   = Map[K, V]
      type OutCT = ClassType[List[Map[K, V]]]
      def tweak(et: MapType[K, V]): ClassType[List[Map[K, V]]] = ListType(et :: Nil filter (_.iri.nonEmpty))
    }

  sealed trait IsOptionEnd[T]
  object IsOptionEnd {
    implicit object head extends IsOptionEnd[Head]
    implicit object last extends IsOptionEnd[Last]
    implicit object max  extends IsOptionEnd[Max]
    implicit object min  extends IsOptionEnd[Min]
    implicit object mean extends IsOptionEnd[Mean]
    implicit object sum  extends IsOptionEnd[Sum]
  }
  implicit def containersOption[End, ET[+Z] <: ClassType[Z], Container, Containers <: HList](
      implicit ev: IsOptionEnd[Container])
    : OutTweaker.Aux[ET[End], Container :: Containers, Option[End], ClassType[Option[End]]] =
    new OutTweaker[ET[End], Container :: Containers] {
      type Out   = Option[End]
      type OutCT = ClassType[Option[End]]
      def tweak(et: ET[End]): ClassType[Option[End]] = OptionType(Some(et) filter (_.iri.nonEmpty))
    }
  implicit def containersCountIs[End, ET[+Z] <: ClassType[Z], Containers <: HList]
    : OutTweaker.Aux[ET[End], Is :: Count :: Containers, Option[End], ClassType[Option[End]]] =
    new OutTweaker[ET[End], Is :: Count :: Containers] {
      type Out   = Option[End]
      type OutCT = ClassType[Option[End]]
      def tweak(et: ET[End]): ClassType[Option[End]] = OptionType(Some(et) filter (_.iri.nonEmpty))
    }
  implicit def containersAnyFilter[ET <: ClassType[_],
                                   Container <: FilterStep,
                                   Containers <: HList,
                                   Out0,
                                   COut0 <: ClassType[_]](implicit out: OutTweaker.Aux[ET, Containers, Out0, COut0])
    : OutTweaker.Aux[ET, Container :: Containers, Out0, COut0] =
    new OutTweaker[ET, Container :: Containers] {
      type Out   = Out0
      type OutCT = COut0
      def tweak(et: ET): COut0 = out.tweak(et)
    }
//  implicit def containersIs[End, ET[+Z] <: ClassType[Z], Containers <: HList, Out, COut <: ClassType[_]] =
//    new OutTweaker[ET[End], Is :: HNil] {
//      type Out   = out.Out
//      type OutCT = out.OutCT
//      def tweak(et: ET[End]): out.OutCT = out.tweak(et)
//    }

  sealed trait IsConstantEnd[T]
  object IsConstantEnd {
    implicit object count extends IsConstantEnd[Count]
  }

  implicit def containersSingle[End, ET[+Z] <: ClassType[Z], Container, Containers <: HList](
      implicit ev: IsConstantEnd[Container]): OutTweaker.Aux[ET[End], Container :: Containers, End, ClassType[End]] =
    new OutTweaker[ET[End], Container :: Containers] {
      type Out   = End
      type OutCT = ClassType[End]
      def tweak(et: ET[End]): ClassType[End] = et //.asInstanceOf[ET[End]]
    }

}
