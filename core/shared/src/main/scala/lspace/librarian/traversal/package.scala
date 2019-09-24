package lspace.librarian

import lspace.librarian.traversal.step.As
import lspace.structure.TypedProperty
import lspace.structure.util._
import shapeless.{::, <:!<, DepFn1, HList, HNil, Poly, Poly1}

import scala.annotation.implicitNotFound

package object traversal {

  type TypedKey[Z] = TypedProperty[Z]

//  object ContainerSteps extends Poly1 {
//    implicit def count                        = at[Count](s => s)
//    implicit def head                         = at[Head](s => s)
//    implicit def last                         = at[Last](s => s)
//    implicit def max                          = at[Max](s => s)
//    implicit def min                          = at[Min](s => s)
//    implicit def mean                         = at[Mean](s => s)
//    implicit def sum                          = at[Sum](s => s)
//    implicit def project[Traversals <: HList] = at[Project[Traversals]](s => s)
//    implicit def group[T <: ClassType[_], Segments <: HList, Tv <: ClassType[_], SegmentsV <: HList] =
//      at[Group[T, Segments, Tv, SegmentsV]](s => s)
//    //  implicit def caseMap[T <: MapStep] = at[T](s => s)
//    implicit def outmap                                      = at[OutMap](s => s)
//    implicit def outemap                                     = at[OutEMap](s => s)
//    implicit def inmap                                       = at[InMap](s => s)
//    implicit def inemap                                      = at[InEMap](s => s)
//    implicit def is                                          = at[Is](s => s)
//    implicit def path[ET <: ClassType[_], Segments <: HList] = at[Path[ET, Segments]](s => s)
//  }

//  object LabelStepToKeyValueLabelStep extends Poly1 {
//    implicit def getType[T, name] = at[As[T, name]](t => t._maphelper ->)
//  }

  /**
    * https://stackoverflow.com/questions/25713668/do-a-covariant-filter-on-an-hlist
    * @tparam L
    * @tparam U
    */
  @implicitNotFound(
    "Implicit not found: lspace.librarian.traversal.CoFilter[${L}, ${U}]. You requested to filter all elements of type <: ${U}, but there is none in the HList ${L}.")
  trait CoFilter[L <: HList, U] extends DepFn1[L] { type Out <: HList }

  object CoFilter {
    def apply[L <: HList, U](implicit f: CoFilter[L, U]): Aux[L, U, f.Out] = f

    type Aux[L <: HList, U, Out0 <: HList] = CoFilter[L, U] { type Out = Out0 }

    implicit def hlistCoFilterHNil[L <: HList, U]: Aux[HNil, U, HNil] =
      new CoFilter[HNil, U] {
        type Out = HNil
        def apply(l: HNil): Out = HNil
      }

    implicit def hlistCoFilter1[U, H <: U, T <: HList](implicit f: CoFilter[T, U]): Aux[H :: T, U, H :: f.Out] =
      new CoFilter[H :: T, U] {
        type Out = H :: f.Out
        def apply(l: H :: T): Out = l.head :: f(l.tail)
      }

    implicit def hlistCoFilter2[U, H, T <: HList](implicit f: CoFilter[T, U], e: H <:!< U): Aux[H :: T, U, f.Out] =
      new CoFilter[H :: T, U] {
        type Out = f.Out
        def apply(l: H :: T): Out = f(l.tail)
      }
  }

  implicit final class HListOps[L <: HList](val l: L) {
    def covariantFilter[U](implicit filter: CoFilter[L, U]): filter.Out = filter(l)
  }

  private def toTuple2[Prefix, Suffix](l: Prefix :: Suffix :: HNil): (Prefix, Suffix) = (l.head, l.tail.head)

  /**
    * Splits an HList L at an element of (super)type U
    * @tparam L The HList to split
    * @tparam U the (super)type which the splitter uses to decide where to split
    */
  @implicitNotFound(
    "Implicit not found: lspace.librarian.traversal.CoSplitLeft[${L}, ${U}]. You requested to split at an element of type <: ${U}, but there is none in the HList ${L}.")
  trait CoSplitLeft[L <: HList, U] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object CoSplitLeft {
    def apply[L <: HList, U](implicit split: CoSplitLeft[L, U]): Aux[L, U, split.Prefix, split.Suffix] = split

    type Aux[L <: HList, U, Prefix0 <: HList, Suffix0 <: HList] = CoSplitLeft[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def splitLeft[L <: HList, U, P <: HList, S <: HList](
        implicit splitLeft: CoSplitLeft0[HNil, L, U, P, S]): Aux[L, U, P, S] =
      new CoSplitLeft[L, U] {
        type Prefix = P
        type Suffix = S

        def product(l: L): Prefix :: Suffix :: HNil = splitLeft(HNil, l)
      }

    trait CoSplitLeft0[AccP <: HList, AccS <: HList, U, P <: HList, S <: HList] extends Serializable {
      def apply(accP: AccP, accS: AccS): P :: S :: HNil
    }

    trait LowPrioritySplitLeft0 {
      implicit def hlistSplitLeft1[AccP <: HList, AccSH, AccST <: HList, U, P <: HList, S <: HList](
          implicit slt: CoSplitLeft0[AccP, AccST, U, P, S]): CoSplitLeft0[AccP, AccSH :: AccST, U, AccSH :: P, S] =
        new CoSplitLeft0[AccP, AccSH :: AccST, U, AccSH :: P, S] {
          def apply(accP: AccP, accS: AccSH :: AccST): (AccSH :: P) :: S :: HNil =
            slt(accP, accS.tail) match {
              case prefix :: suffix :: HNil => (accS.head :: prefix) :: suffix :: HNil
            }
        }
      implicit def hlistSplitLeft1a[AccP <: HList, AccSH, U]: CoSplitLeft0[AccP, HList, U, HList, HNil] =
        new CoSplitLeft0[AccP, HList, U, HList, HNil] {
          def apply(accP: AccP, accS: HList): HList :: HNil :: HNil = (accS) :: HNil :: HNil
        }
    }

    object CoSplitLeft0 extends LowPrioritySplitLeft0 {
      implicit def hlistSplitLeft2[P <: HList, SH, SH0 <: SH, ST <: HList]
        : CoSplitLeft0[P, SH0 :: ST, SH, P, SH0 :: ST] =
        new CoSplitLeft0[P, SH0 :: ST, SH, P, SH0 :: ST] {
          def apply(accP: P, accS: SH0 :: ST): P :: (SH0 :: ST) :: HNil = accP :: accS :: HNil
        }
    }
  }

  /**
    * Type Class witnessing that an 'HList' can be spanned with a 'Poly' to produce two 'HList's
    *
    * @author Thijs Broersen
    */
  @implicitNotFound(
    "Implicit not found: lspace.librarian.traversal.Span[${L}, ${U}]. You requested to span all first consecutive elements of Poly ${U}, but there is none in the HList ${L}.")
  trait Span[L <: HList, U <: Poly] extends DepFn1[L] with Serializable {
    type Prefix <: HList
    type Suffix <: HList
    type Out = (Prefix, Suffix)

    def apply(l: L): Out = toTuple2(product(l))
    def product(l: L): Prefix :: Suffix :: HNil
  }

  object Span {
    import shapeless.poly._

    def apply[L <: HList, U <: Poly](implicit span: Span[L, U]): Aux[L, U, span.Prefix, span.Suffix] = span

    type Aux[L <: HList, U <: Poly, Prefix0 <: HList, Suffix0 <: HList] = Span[L, U] {
      type Prefix = Prefix0
      type Suffix = Suffix0
    }

    implicit def spanLeft[H, L <: HList, U <: Poly, ClrResult, P <: HList, S <: HList](
        implicit
        clr: Case1.Aux[U, H, ClrResult],
        spanLeft: Span0[HNil, H :: L, U, P, S]): Aux[H :: L, U, P, S] =
      new Span[H :: L, U] {
        type Prefix = P
        type Suffix = S

        def product(l: H :: L): Prefix :: Suffix :: HNil = spanLeft(HNil, l)
      }

    trait Span0[AccP <: HList, AccS <: HList, U <: Poly, P <: HList, S <: HList] extends Serializable {
      def apply(accP: AccP, accS: AccS): P :: S :: HNil
    }

    trait LowPrioritySpan0 {
      implicit def hlistSpan1[AccP <: HList, AccSH, AccST <: HList, U <: Poly, P <: HList, S <: HList, ClrResult](
          implicit slt: Span0[AccP, AccST, U, P, S]): Span0[AccP, AccSH :: AccST, U, AccSH :: P, S] =
        new Span0[AccP, AccSH :: AccST, U, AccSH :: P, S] {
          def apply(accP: AccP, accS: AccSH :: AccST): (AccSH :: P) :: S :: HNil =
            slt(accP, accS.tail) match {
              case prefix :: suffix :: HNil => (accS.head :: prefix) :: suffix :: HNil
            }
        }
    }

    object Span0 extends LowPrioritySpan0 {
      implicit def hlistSpan2[P <: HList, U <: Poly, SH, ST <: HList, Result <: HList](
          implicit
          collect: shapeless.ops.hlist.Collect.Aux[SH :: HNil, U, Result],
          ev: Result =:= HNil): Span0[P, SH :: ST, U, P, SH :: ST] =
        new Span0[P, SH :: ST, U, P, SH :: ST] {
          def apply(accP: P, accS: SH :: ST): P :: (SH :: ST) :: HNil = accP :: accS :: HNil
        }

      implicit def hlistSpan0[P <: HList, U <: Poly, ST <: HList](
          implicit
          ev: ST =:= HNil): Span0[P, ST, U, P, ST] =
        new Span0[P, ST, U, P, ST] {
          def apply(accP: P, accS: ST): P :: ST :: HNil = accP :: accS :: HNil
        }
      implicit def hlistSpan1[P <: HList, U <: Poly, ST <: HList](
          implicit
          ev: ST =:= HList): Span0[P, ST, U, P, ST] =
        new Span0[P, ST, U, P, ST] {
          def apply(accP: P, accS: ST): P :: ST :: HNil = accP :: accS :: HNil
        }
    }
  }
}
