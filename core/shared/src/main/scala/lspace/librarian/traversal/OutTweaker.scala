package lspace.librarian.traversal

import lspace.datatype.{CollectionType, ListType, MapType, TupleType}
import lspace.librarian.traversal.step.{Count, Group, Head, Last, Max, Min, Project}
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
  implicit def nocontainers[End, ET[+Z] <: ClassType[Z]] = new OutTweaker[ET[End], HNil] {
    type Out   = List[End]
    type OutCT = ListType[End]
    def tweak(et: ET[End]): ListType[End] = ListType(et :: Nil filter (_.iri.nonEmpty))
  }
  //  implicit def containers[ET, SET, Container, Containers <: HList] = new OutTweaker[ET, SET, Container :: Containers] {
  //    type Out = SET
  //    def tweak[CT <: ClassType[SET]](ct1: ET, ct2: CT): ClassType[Out] = ct2
  //  }
  implicit def containersMap[K, V, Container, Containers <: HList](implicit ev: Container <:< Group[_, _, _, _]) =
    new OutTweaker[MapType[K, V], Container :: Containers] {
      type Out   = Map[K, V]
      type OutCT = MapType[K, V]
      def tweak(et: MapType[K, V]): MapType[K, V] =
        MapType[K, V](
          et.asInstanceOf[TupleType[(K, V)]].rangeTypes.head.asInstanceOf[List[ClassType[K]]] filter (_.iri.nonEmpty),
          et.asInstanceOf[TupleType[(K, V)]]
            .rangeTypes
            .tail
            .head
            .asInstanceOf[List[ClassType[V]]] filter (_.iri.nonEmpty)
        )
    }
  sealed trait IsListEnd[T]
  object IsListEnd {
    implicit def project[T <: HList]   = new IsListEnd[Project[T]] {}
    implicit def mapstep[T <: MapStep] = new IsListEnd[T]          {}
  }
  implicit def containersList[End, ET[+Z] <: ClassType[Z], Container, Containers <: HList](
      implicit ev: IsListEnd[Container]) =
    new OutTweaker[ET[End], Container :: Containers] {
      type Out   = List[End]
      type OutCT = ListType[End]
      def tweak(et: ET[End]): ListType[End] = ListType(et :: Nil filter (_.iri.nonEmpty))
    }
  implicit def containersList2[K, V, Container, Containers <: HList](implicit ev: IsListEnd[Container]) =
    new OutTweaker[MapType[K, V], Container :: Containers] {
      type Out   = Map[K, V]
      type OutCT = ListType[Map[K, V]]
      def tweak(et: MapType[K, V]): ListType[Map[K, V]] = ListType(et :: Nil filter (_.iri.nonEmpty))
    }

  sealed trait IsOptionEnd[T]
  object IsOptionEnd {
    implicit object head extends IsOptionEnd[Head]
    implicit object last extends IsOptionEnd[Last]
    implicit object max  extends IsOptionEnd[Max]
    implicit object min  extends IsOptionEnd[Min]
  }
  implicit def containersOption[End, ET[+Z] <: ClassType[Z], Container, Containers <: HList](
      implicit ev: IsOptionEnd[Container]) =
    new OutTweaker[ET[End], Container :: Containers] {
      type Out   = Option[End]
      type OutCT = ET[End]
      def tweak(et: ET[End]): ET[End] = et.asInstanceOf[ET[End]]
    }

  sealed trait IsConstantEnd[T]
  object IsConstantEnd {
    implicit object count extends IsConstantEnd[Count]
  }

  implicit def containersSingle[End, ET[+Z] <: ClassType[Z], Container, Containers <: HList](
      implicit ev: IsConstantEnd[Container]) =
    new OutTweaker[ET[End], Container :: Containers] {
      type Out   = End
      type OutCT = ET[End]
      def tweak(et: ET[End]): ET[End] = et //.asInstanceOf[ET[End]]
    }

}
