package lspace.librarian.traversal

import lspace.datatype.{ListType, MapType, TupleType}
import lspace.librarian.traversal.step.{Group, Project}
import lspace.structure.ClassType
import shapeless.{::, <:!<, HList, HNil, Poly2}

sealed trait OutTweaker[ET, SET, Containers <: HList] {
  type Out

  def tweak[CT <: ClassType[SET]](ct1: ET, ct2: CT): ClassType[Out]
}

object OutTweaker extends Poly2 {
  type Aux[ET <: ClassType[_], SET, Containers <: HList, Out0] = OutTweaker[ET, SET, Containers] { type Out = Out0 }
  implicit def nocontainers[End, ET[Z] <: ClassType[Z], SET] = new OutTweaker[ET[End], SET, HNil] {
    type Out = End
    def tweak[CT <: ClassType[SET]](ct1: ET[End], ct2: CT): ClassType[Out] = ct1
  }
//  implicit def containers[ET, SET, Container, Containers <: HList] = new OutTweaker[ET, SET, Container :: Containers] {
//    type Out = SET
//    def tweak[CT <: ClassType[SET]](ct1: ET, ct2: CT): ClassType[Out] = ct2
//  }
  implicit def containersMap[ET, K, V, Container, Containers <: HList](implicit ev: Container <:< Group[_, _, _, _]) =
    new OutTweaker[ET, Map[K, V], Container :: Containers] {
      type Out = (K, V)
      def tweak[CT <: ClassType[Map[K, V]]](ct1: ET, ct2: CT): ClassType[Out] =
        TupleType[Out](List(ct2.asInstanceOf[MapType[K, V]].keyRange, ct2.asInstanceOf[MapType[K, V]].valueRange))
    }
  implicit def containersTuple[ET, T, Container, Containers <: HList](implicit ev: Container <:< Project[_]) =
    new OutTweaker[ET, List[T], Container :: Containers] {
      type Out = T
      def tweak[CT <: ClassType[List[T]]](ct1: ET, ct2: CT): ClassType[Out] =
        ct2.asInstanceOf[ListType[T]].valueRange.head
    }
  implicit def containersOthers[ET, SET, Container, Containers <: HList](implicit ev: Container <:!< Group[_, _, _, _],
                                                                         ev2: Container <:!< Project[_]) =
    new OutTweaker[ET, SET, Container :: Containers] {
      type Out = SET
      def tweak[CT <: ClassType[SET]](ct1: ET, ct2: CT): ClassType[Out] = ct2
    }
}
