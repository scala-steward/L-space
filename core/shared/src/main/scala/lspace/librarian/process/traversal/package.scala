package lspace.librarian.process

import lspace.librarian.datatype._
import lspace.librarian.process.traversal.helper._
import lspace.librarian.process.traversal.step._
import lspace.librarian.structure._
import shapeless.{Path => _, _}
import shapeless.{::, DepFn2, HList, HNil, Lazy, Poly2}

package object traversal {

  type TypedKey[Z] = TypedProperty[Z]

  //  object TraversalResult {
  //    implicit def toList[T](t: TraversalResult[T]): List[T] = t.values
  //  }
  //  case class TraversalResult[T](values: List[T], dt: ClassType[T]) {
  //    //    def map[R](f: T => R) = values.map(f)
  //  }

  object ContainerSteps extends Poly1 {
    implicit def group[T <: ClassType[_]] = at[Group[T]](s => s)
    //  implicit def caseMap[T <: MapStep] = at[T](s => s)
    implicit def outmap  = at[OutMap](s => s)
    implicit def outemap = at[OutEMap](s => s)
    implicit def inmap   = at[InMap](s => s)
    implicit def inemap  = at[InEMap](s => s)
    implicit def path    = at[Path](s => s)
  }

  sealed trait StructureCalculator[L <: HList, CT <: ClassType[_]] {
    type Out
    type OutCT <: ClassType[_]

    def convert(hlist: L, value: CT): List[OutCT]
  }

  object StructureCalculator {

    type Aux[L <: HList, CT <: ClassType[_], Out0, OutCT0 <: ClassType[_]] =
      StructureCalculator[L, CT] {
        type Out   = Out0
        type OutCT = OutCT0
      }

    private trait Impl[L <: HList, CT <: ClassType[_], Out0, OutCT0 <: ClassType[_]]
        extends StructureCalculator[L, CT] {
      override type Out   = Out0
      override type OutCT = OutCT0
    }

    implicit def hnil[CT <: ClassType[_], Tout, CTout <: ClassType[_]](
        implicit clsTpbl: ClassTypeable.Aux[CT, Tout, CTout]): Aux[HNil, CT, Tout, CTout] =
      new Impl[HNil, CT, Tout, CTout] {
        override def convert(hlist: HNil, value: CT): List[OutCT] =
          List(clsTpbl.ct).filter(_.iri.nonEmpty) //.asInstanceOf[OutCT] //.asInstanceOf[CT]
      }

    implicit def hnil2[CT <: ClassType[_], Tout, CTout <: ClassType[_]](
        implicit clsTpbl: ClassTypeable.Aux[CT, Tout, CTout]): Aux[HNil.type, CT, Tout, CTout] =
      new Impl[HNil.type, CT, Tout, CTout] {
        override def convert(hlist: HNil.type, value: CT): List[OutCT] =
          List(clsTpbl.ct)
            .filter(_.iri.nonEmpty) //.asInstanceOf[OutCT] //ClassType.valueToOntologyResource(value).asInstanceOf[CT]
      }

    //    implicit def mapStep[L <: HList, T, Step <: MapStep](implicit inner: MyLeftFolder[L, T]): Aux[Step :: L, T, Map[Property, inner.Out]] = new Impl[Step :: L, T, Map[Property, inner.Out]] {
    //      override def convert(hlist: Step :: L, value: T): Map[Property, inner.Out] = {
    //        val im = inner.convert(hlist.tail, value)
    //        Map(Property("") -> im)
    //      }
    //    }
    implicit def outmapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[OutMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] =
      new Impl[OutMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] {
        override def convert(hlist: OutMap :: L, value: CT): List[CollectionType[Map[Property, List[inner.Out]]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    List(ListType(im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))))
              .asInstanceOf[CollectionType[Map[Property, List[inner.Out]]]])
        }
      }
    implicit def outemapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[OutEMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] =
      new Impl[OutEMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] {
        override def convert(hlist: OutEMap :: L, value: CT): List[CollectionType[Map[Property, List[inner.Out]]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    List(ListType(im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))))
              .asInstanceOf[CollectionType[Map[Property, List[inner.Out]]]])
        }
      }
    implicit def inmapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[InMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] =
      new Impl[InMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] {
        override def convert(hlist: InMap :: L, value: CT): List[CollectionType[Map[Property, List[inner.Out]]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    List(ListType(im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))))
              .asInstanceOf[CollectionType[Map[Property, List[inner.Out]]]])
        }
      }
    implicit def inemapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[InEMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] =
      new Impl[InEMap :: L, CT, Map[Property, List[inner.Out]], CollectionType[Map[Property, List[inner.Out]]]] {
        override def convert(hlist: InEMap :: L, value: CT): List[CollectionType[Map[Property, List[inner.Out]]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    List(ListType(im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))))
              .asInstanceOf[CollectionType[Map[Property, List[inner.Out]]]])
        }
      }

    implicit def groupStep[L <: HList, CT <: ClassType[_], AT <: ClassType[_], Aout, ATOut <: ClassType[_]](
        implicit inner: StructureCalculator[L, CT],
        clsTpblA: ClassTypeable.Aux[AT, Aout, ATOut])
      : Aux[Group[AT] :: L, CT, Map[List[Aout], List[inner.Out]], CollectionType[Map[List[Aout], List[inner.Out]]]] =
      new Impl[Group[AT] :: L, CT, Map[List[Aout], List[inner.Out]], CollectionType[Map[List[Aout], List[inner.Out]]]] {
        override def convert(hlist: Group[AT] :: L,
                             value: CT): List[CollectionType[Map[List[Aout], List[inner.Out]]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(ListType(List(clsTpblA.ct.asInstanceOf[ClassType[Aout]]))),
                    List(ListType(im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))))
              .asInstanceOf[CollectionType[Map[List[Aout], List[inner.Out]]]])
        }
      }

    implicit def pathStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[Path :: L, CT, List[inner.Out], CollectionType[List[inner.Out]]] =
      new Impl[Path :: L, CT, List[inner.Out], CollectionType[List[inner.Out]]] {
        override def convert(hlist: Path :: L, value: CT): List[CollectionType[List[inner.Out]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            ListType(im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))
              .asInstanceOf[CollectionType[List[inner.Out]]])
        }
      }
  }

  object TraversalsFolder extends Poly2 {
    implicit def getType[Start1,
                         ST1[+Z] <: ClassType[Z],
                         End1,
                         ET1[+Z] <: ClassType[Z],
                         Steps1 <: HList,
                         Start2,
                         ST2[+Z] <: ClassType[Z],
                         End2,
                         ET2[+Z] <: ClassType[Z],
                         Steps2 <: HList] =
      at((t1: Traversal[ST1[Start1], ET1[End1], Steps1], t2: Traversal[ST2[Start2], ET2[End2], Steps2]) =>
        List(t1, t2).head)
  }

  object EndFolder extends Poly2 {
    implicit def concat[End1, ET1[+Z] <: ClassType[Z], End2, ET2[+Z] <: ClassType[Z]] =
      at[ET1[End1], ET2[End2]]((tl: ET1[End1], t2: ET2[End2]) => t2 :: tl :: Nil head)
  }

  object TraveralEndMapper extends Poly1 {
    implicit def mapET[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList] = at[Traversal[ST, ET, Steps]](t => t.et)
  }

  object LabelSteps extends Poly1 {
    implicit def as[T, name <: String] = at[As[T, name]](s => s)
  }

  object LabelStepTypes extends Poly1 {
    implicit def getType[T, name <: String] = at[As[T, name]](t => t._maphelper)
  }

//  object LabelStepToKeyValueLabelStep extends Poly1 {
//    implicit def getType[T, name] = at[As[T, name]](t => t._maphelper ->)
//  }

  trait SelectorSelecter[T <: HList] extends DepFn1[T] {
    type Out <: Selector[T, HNil]
  }
  object SelectorSelecter {
    import shapeless.ops.hlist.{Selector => _, _}

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

  /**
    * https://stackoverflow.com/questions/25713668/do-a-covariant-filter-on-an-hlist
    * @tparam L
    * @tparam U
    */
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
}
