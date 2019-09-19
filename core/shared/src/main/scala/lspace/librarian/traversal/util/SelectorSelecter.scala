package lspace.librarian.traversal.util

import lspace.librarian.traversal.step.As
import shapeless.{::, DepFn1, HList, HNil}

trait SelectorSelecter[T <: HList] extends DepFn1[T] {
  type Out <: Selector[T, HNil]
}

object SelectorSelecter {
  import shapeless.ops.hlist.{Selector => _}

  //    def apply[L <: HList](implicit selector: SelectorSelecter[L]): Aux[L, selector.Out] = selector

  type Aux[In <: HList, Out0 <: Selector[_, HNil]] = SelectorSelecter[In] { type Out = Out0 }

  implicit def selector3Selecter[A, nameA <: String, B, nameB <: String, C, nameC <: String]
    : Aux[As[C, nameC] :: As[B, nameB] :: As[A, nameA] :: HNil, Selector3[A, nameA, B, nameB, C, nameC, HNil]] =
    new SelectorSelecter[As[C, nameC] :: As[B, nameB] :: As[A, nameA] :: HNil] {
      type Out = Selector3[A, nameA, B, nameB, C, nameC, HNil]
      def apply(l: As[C, nameC] :: As[B, nameB] :: As[A, nameA] :: HNil) =
        Selector3.apply(l, HNil)
    }
  implicit def selector2Selecter[A, nameA <: String, B, nameB <: String]
    : Aux[As[B, nameB] :: As[A, nameA] :: HNil, Selector2[A, nameA, B, nameB, HNil]] =
    new SelectorSelecter[As[B, nameB] :: As[A, nameA] :: HNil] {
      type Out = Selector2[A, nameA, B, nameB, HNil]
      def apply(l: As[B, nameB] :: As[A, nameA] :: HNil) = Selector2.apply(l, HNil)
    }

  implicit def selector1Selecter[A, nameA <: String]: Aux[As[A, nameA] :: HNil, Selector1[A, nameA, HNil]] =
    new SelectorSelecter[As[A, nameA] :: HNil] {
      type Out = Selector1[A, nameA, HNil]
      def apply(l: As[A, nameA] :: HNil) = Selector1.apply(l, HNil)
    }

  implicit def selector0Selecter: Aux[HNil, Selector0[HNil]] = new SelectorSelecter[HNil] {
    type Out = Selector0[HNil]
    def apply(l: HNil): Out = Selector0(l, HNil)
  }
}
