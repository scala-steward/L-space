package lspace.librarian.traversal.util

import lspace.librarian.traversal.step.As
import lspace.librarian.traversal.step.Select.Selection
import shapeless.{::, HList, HNil, LUBConstraint}
import shapeless.ops.hlist.{Mapper, Reverse, Tupler}

trait Selector[+L <: HList, SL <: HList] {
  def l: L
  def sl: SL
}

object Selector {
  //  implicit def labelsToS(l: HNil) = Selector0(l, HNil)
  //  implicit def labelsToS1[A](l: A :: HNil) = Selector1(l, HNil)
  //  implicit def labelsToS2[A, B](l: B :: A :: HNil) = Selector2(l, HNil)
  //  implicit def labelsToS3[A, B, C](l: C :: B :: A :: HNil) = Selector3(l, HNil)
  //
  //  def apply(l: HNil) = Selector0(l, HNil)
  //  def apply[A](l: A ::  HNil) = Selector1(l, HNil)
  //  def apply[A, B](l: B :: A :: HNil) = Selector2(l, HNil)
  //  def apply[A, B, C](l: C :: B :: A :: HNil) = Selector3(l, HNil)

  //  implicit def selectorToSelectionx[Labels <: HList, SelectedLabels <: HList, S <: Selector[Labels, SelectedLabels], SelectedTypes <: HList, TypesTuple](
  //    selector: S)(
  //    implicit
  //    lub: LUBConstraint[SelectedLabels, As[_]],
  //    mapper: Mapper.Aux[astypes.type, SelectedLabels, SelectedTypes],
  //    tupler: Tupler.Aux[SelectedTypes, TypesTuple]): Selection[SelectedLabels, TypesTuple] = Selection[SelectedLabels, TypesTuple](selector.sl)
}

object Selector0 {
  //  implicit def labelsToS(l: HNil) = Selector0(l)
}
case class Selector0[SelectedLabels <: HList](l: HNil, sl: SelectedLabels) extends Selector[HNil, SelectedLabels]
object Selector1 {
  //  implicit def labelsToS[A](l: A :: HNil) = Selector1(l)
  implicit def selectorToSelection1[A, nameA <: String, SelectedTypes <: HList](
    selector: Selector1[A, nameA, As[A, nameA] :: HNil]
  )(implicit
    lub: LUBConstraint[As[A, nameA] :: HNil, As[_, _]],
    mapper: Mapper.Aux[LabelStepTypes.type, As[A, nameA] :: HNil, SelectedTypes]
  ): Selection[As[A, nameA] :: HNil, A] =
    Selection[As[A, nameA] :: HNil, A](selector.sl)
}
case class Selector1[A, nameA <: String, SelectedLabels <: HList](l: As[A, nameA] :: HNil, sl: SelectedLabels)
    extends Selector[As[A, nameA] :: HNil, SelectedLabels] {
  def a = Selection[As[A, nameA] :: SelectedLabels, A](l.head :: sl)
}
object Selector2 {

  implicit def selector2ToSelection2[
    A,
    Aname <: String,
    B,
    Bname <: String,
    As1,
    As1name <: String,
    As2,
    As2name <: String,
    RLabels <: HList,
    SelectedTypes <: HList,
    TypesTuple
  ](selector: Selector2[A, Aname, B, Bname, As[As1, As1name] :: As[As2, As2name] :: HNil])(implicit
    reverse: Reverse.Aux[As[As1, As1name] :: As[As2, As2name] :: HNil, RLabels],
    lub: LUBConstraint[RLabels, As[_, _]],
    mapper: Mapper.Aux[LabelStepTypes.type, RLabels, SelectedTypes],
    tupler: Tupler.Aux[SelectedTypes, TypesTuple]
  ): Selection[As[As1, As1name] :: As[As2, As2name] :: HNil, TypesTuple] =
    Selection[As[As1, As1name] :: As[As2, As2name] :: HNil, TypesTuple](selector.sl)

  implicit def selector2ToSelection1[
    A,
    Aname <: String,
    B,
    Bname <: String,
    As1,
    As1name <: String,
    SelectedTypes <: HList,
    TypesTuple
  ](selector: Selector2[A, Aname, B, Bname, As[As1, As1name] :: HNil])(implicit
    lub: LUBConstraint[As[As1, As1name] :: HNil, As[_, _]],
    mapper: Mapper.Aux[LabelStepTypes.type, As[As1, As1name] :: HNil, SelectedTypes]
  ): Selection[As[As1, As1name] :: HNil, As1] =
    Selection[As[As1, As1name] :: HNil, As1](selector.sl)

}
case class Selector2[A, Aname <: String, B, Bname <: String, SelectedLabels <: HList](
  l: As[B, Bname] :: As[A, Aname] :: HNil,
  sl: SelectedLabels
) extends Selector[As[B, Bname] :: As[A, Aname] :: HNil, SelectedLabels] {
  def b = Selector2(l, l.head :: sl)
  def a = Selector2(l, l.tail.head :: sl)
}
object Selector3 {

  implicit def selector3ToSelection3[
    A,
    Aname <: String,
    B,
    Bname <: String,
    C,
    Cname <: String,
    As1,
    As1name <: String,
    As2,
    As2name <: String,
    As3,
    As3name <: String,
    RLabels <: HList,
    SelectedTypes <: HList,
    TypesTuple
  ](
    selector: Selector3[A, Aname, B, Bname, C, Cname, As[As1, As1name] :: As[As2, As2name] :: As[As3, As3name] :: HNil]
  )(implicit
    reverse: Reverse.Aux[As[As1, As1name] :: As[As2, As2name] :: As[As3, As3name] :: HNil, RLabels],
    lub: LUBConstraint[RLabels, As[_, _]],
    mapper: Mapper.Aux[LabelStepTypes.type, RLabels, SelectedTypes],
    tupler: Tupler.Aux[SelectedTypes, TypesTuple]
  ): Selection[As[As1, As1name] :: As[As2, As2name] :: As[As3, As3name] :: HNil, TypesTuple] =
    Selection[As[As1, As1name] :: As[As2, As2name] :: As[As3, As3name] :: HNil, TypesTuple](selector.sl)
  implicit def selector3ToSelection2[
    A,
    Aname <: String,
    B,
    Bname <: String,
    C,
    Cname <: String,
    As1,
    As1name <: String,
    As2,
    As2name <: String,
    RLabels <: HList,
    SelectedTypes <: HList,
    TypesTuple
  ](selector: Selector3[A, Aname, B, Bname, C, Cname, As[As1, As1name] :: As[As2, As2name] :: HNil])(implicit
    reverse: Reverse.Aux[As[As1, As1name] :: As[As2, As2name] :: HNil, RLabels],
    lub: LUBConstraint[RLabels, As[_, _]],
    mapper: Mapper.Aux[LabelStepTypes.type, RLabels, SelectedTypes],
    tupler: Tupler.Aux[SelectedTypes, TypesTuple]
  ): Selection[As[As1, As1name] :: As[As2, As2name] :: HNil, TypesTuple] =
    Selection[As[As1, As1name] :: As[As2, As2name] :: HNil, TypesTuple](selector.sl)
  implicit def selector3ToSelection1[
    A,
    Aname <: String,
    B,
    Bname <: String,
    C,
    Cname <: String,
    As1,
    As1name <: String,
    SelectedTypes <: HList,
    TypesTuple
  ](selector: Selector3[A, Aname, B, Bname, C, Cname, As[As1, As1name] :: HNil])(implicit
    lub: LUBConstraint[As[As1, As1name] :: HNil, As[_, _]],
    mapper: Mapper.Aux[LabelStepTypes.type, As[As1, As1name] :: HNil, SelectedTypes]
  ): Selection[As[As1, As1name] :: HNil, As1] =
    Selection[As[As1, As1name] :: HNil, As1](selector.sl)
}
case class Selector3[A, Aname <: String, B, Bname <: String, C, Cname <: String, SelectedLabels <: HList](
  l: As[C, Cname] :: As[B, Bname] :: As[A, Aname] :: HNil,
  sl: SelectedLabels
) extends Selector[As[C, Cname] :: As[B, Bname] :: As[A, Aname] :: HNil, SelectedLabels] {
  def c = Selector3(l, l.head :: sl)
  def b = Selector3(l, l.tail.head :: sl)
  def a = Selector3(l, l.tail.tail.head :: sl)
}
