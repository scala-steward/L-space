package lspace.librarian

import lspace.datatype._
import lspace.librarian.traversal.step._
import lspace.structure._
import lspace.structure.util._
import monix.reactive.Observable
import shapeless.ops.hlist.{Collect, Reverse}
import shapeless.{Path => _, Segment => _, _}
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
    implicit def head                                        = at[Head](s => s)
    implicit def last                                        = at[Last](s => s)
    implicit def project[Traversals <: HList]                = at[Project[Traversals]](s => s)
    implicit def group[T <: ClassType[_], Segments <: HList] = at[Group[T, Segments]](s => s)
    //  implicit def caseMap[T <: MapStep] = at[T](s => s)
    implicit def outmap                                      = at[OutMap](s => s)
    implicit def outemap                                     = at[OutEMap](s => s)
    implicit def inmap                                       = at[InMap](s => s)
    implicit def inemap                                      = at[InEMap](s => s)
    implicit def path[ET <: ClassType[_], Segments <: HList] = at[Path[ET, Segments]](s => s)
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
        implicit clsTpbl: ClassTypeable.Aux[CT, Tout, CTout]): Aux[HNil, CT, List[Tout], ListType[Tout]] =
      new Impl[HNil, CT, List[Tout], ListType[Tout]] {
        override def convert(hlist: HNil, value: CT): List[ListType[Tout]] =
          List(ListType[Tout](List(clsTpbl.ct.asInstanceOf[ClassType[Tout]]).filter(_.iri.nonEmpty))) //.asInstanceOf[List[ListType[Tout]]]
      }

    implicit def hnil2[CT <: ClassType[_], Tout, CTout <: ClassType[_]](
        implicit clsTpbl: ClassTypeable.Aux[CT, Tout, CTout]): Aux[HNil.type, CT, List[Tout], ListType[Tout]] =
      new Impl[HNil.type, CT, List[Tout], ListType[Tout]] {
        override def convert(hlist: HNil.type, value: CT): List[ListType[Tout]] =
          List(ListType[Tout](List(clsTpbl.ct.asInstanceOf[ClassType[Tout]]).filter(_.iri.nonEmpty))) //.asInstanceOf[OutCT] //ClassType.valueToOntologyResource(value).asInstanceOf[CT]
      }

    implicit def head[CT <: ClassType[_], Tout, CTout <: ClassType[_]](
        implicit clsTpbl: ClassTypeable.Aux[CT, Tout, CTout]): Aux[Head :: HNil, CT, Tout, CTout] =
      new Impl[Head :: HNil, CT, Tout, CTout] {
        override def convert(hlist: Head :: HNil, value: CT): List[CTout] =
          List(clsTpbl.ct).filter(_.iri.nonEmpty)
      }
    implicit def last[CT <: ClassType[_], Tout, CTout <: ClassType[_]](
        implicit clsTpbl: ClassTypeable.Aux[CT, Tout, CTout]): Aux[Last :: HNil, CT, Tout, CTout] =
      new Impl[Last :: HNil, CT, Tout, CTout] {
        override def convert(hlist: Last :: HNil, value: CT): List[CTout] =
          List(clsTpbl.ct).filter(_.iri.nonEmpty)
      }

    //    implicit def mapStep[L <: HList, T, Step <: MapStep](implicit inner: MyLeftFolder[L, T]): Aux[Step :: L, T, Map[Property, inner.Out]] = new Impl[Step :: L, T, Map[Property, inner.Out]] {
    //      override def convert(hlist: Step :: L, value: T): Map[Property, inner.Out] = {
    //        val im = inner.convert(hlist.tail, value)
    //        Map(Property("") -> im)
    //      }
    //    }
    implicit def outmapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[OutMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] =
      new Impl[OutMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] {
        override def convert(hlist: OutMap :: L, value: CT): List[CollectionType[Map[Property, inner.Out]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))
              .asInstanceOf[CollectionType[Map[Property, inner.Out]]])
        }
      }
    implicit def outemapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[OutEMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] =
      new Impl[OutEMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] {
        override def convert(hlist: OutEMap :: L, value: CT): List[CollectionType[Map[Property, inner.Out]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))
              .asInstanceOf[CollectionType[Map[Property, inner.Out]]])
        }
      }
    implicit def inmapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[InMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] =
      new Impl[InMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] {
        override def convert(hlist: InMap :: L, value: CT): List[CollectionType[Map[Property, inner.Out]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))
              .asInstanceOf[CollectionType[Map[Property, inner.Out]]])
        }
      }
    implicit def inemapStep[L <: HList, CT <: ClassType[_]](implicit inner: StructureCalculator[L, CT])
      : Aux[InEMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] =
      new Impl[InEMap :: L, CT, Map[Property, inner.Out], CollectionType[Map[Property, inner.Out]]] {
        override def convert(hlist: InEMap :: L, value: CT): List[CollectionType[Map[Property, inner.Out]]] = {
          val im = inner.convert(hlist.tail, value)
          List(
            MapType(List(DataType.default.`@property`),
                    im.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))
              .asInstanceOf[CollectionType[Map[Property, inner.Out]]])
        }
      }

    implicit def groupStep[L <: HList,
                           CT <: ClassType[_],
                           AT <: ClassType[_],
                           Segments <: HList,
                           Steps <: HList,
                           RSteps <: HList,
                           Containers <: HList](
        implicit
        flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
        reverse: Reverse.Aux[Steps, RSteps],
        f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
        innerKey: StructureCalculator[Containers, AT],
        inner: StructureCalculator[L, CT])
      : Aux[Group[AT, Segments] :: L, CT, Map[innerKey.Out, inner.Out], CollectionType[Map[innerKey.Out, inner.Out]]] =
      new Impl[Group[AT, Segments] :: L, CT, Map[innerKey.Out, inner.Out], CollectionType[Map[innerKey.Out, inner.Out]]] {
        override def convert(hlist: Group[AT, Segments] :: L,
                             value: CT): List[CollectionType[Map[innerKey.Out, inner.Out]]] = {
          val ik = innerKey.convert(f(reverse(flat(hlist.head.by.segments))), hlist.head.by.et)
          val iv = inner.convert(hlist.tail, value)
          List(
            MapType(ik.asInstanceOf[List[ClassType[innerKey.Out]]].filter(_.iri.nonEmpty),
                    iv.asInstanceOf[List[ClassType[inner.Out]]].filter(_.iri.nonEmpty))
              .asInstanceOf[CollectionType[Map[innerKey.Out, inner.Out]]]
          )
        }
      }

    object TraversalsMapper extends Poly1 {
      implicit def getSteps[ST <: ClassType[_],
                            ET <: ClassType[_],
                            Segments <: HList,
                            Steps <: HList,
                            RSteps <: HList,
                            Containers <: HList](
          implicit
          flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
          reverse: Reverse.Aux[Steps, RSteps],
          f: Collect.Aux[RSteps, ContainerSteps.type, Containers]
//          innerKey: StructureCalculator[Containers, ET]
      ) =
        at[Traversal[ST, ET, Segments]](s => s.et :: f(reverse(flat(s.segments))))
//          innerKey.convert(f(reverse(flat(s.segments))), s.et).filter(_.iri.nonEmpty))
    }
    object TraversalsMapper2 extends Poly1 {
      implicit def getSteps[ST <: ClassType[_],
                            ET <: ClassType[_],
                            Segments <: HList,
                            Steps <: HList,
                            RSteps <: HList,
                            Containers <: HList](
          implicit
          flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
          reverse: Reverse.Aux[Steps, RSteps],
          f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
          innerKey: StructureCalculator[Containers, ET]
      ) =
        at[Traversal[ST, ET, Segments]](s => innerKey.convert(f(reverse(flat(s.segments))), s.et))
    }

    object TraversalsMapper3 extends Poly1 {
      implicit def getSteps[ST <: ClassType[_],
                            ET <: ClassType[_],
                            Segments <: HList,
                            Steps <: HList,
                            RSteps <: HList,
                            Containers <: HList](
          implicit
          flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
          reverse: Reverse.Aux[Steps, RSteps],
          f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
          innerKey: StructureCalculator[Containers, ET]
      ) =
        at[Traversal[ST, ET, Segments]](s => s.asInstanceOf[innerKey.Out])
    }

    object ContainersToCTMapper extends Poly1 {
      implicit def classType[ET <: ClassType[_], Containers <: HList](
          implicit
          inner: StructureCalculator[Containers, ET]) =
        at[ET :: Containers] { s =>
          inner.convert(s.tail, s.head)
        } //this cast is never executed runtime but only used for type-calculation
    }
    object ContainersToOutMapper extends Poly1 {
      implicit def classType[Containers <: HList, ET <: ClassType[_]](
          implicit inner: StructureCalculator[Containers, ET]) =
        at[ET :: Containers](s => s.asInstanceOf[inner.Out]) //this cast is never executed runtime but only used for type-calculation
    }

    implicit def projectStep[L <: HList,
//                             CT <: ClassType[_],
                             Traversals <: HList,
                             Containers <: HList,
                             CTout <: HList,
                             Tout <: HList,
                             Out](
        implicit
        mapper: shapeless.ops.hlist.Mapper.Aux[TraversalsMapper.type, Traversals, Containers],
        outCtMapper: shapeless.ops.hlist.Mapper.Aux[TraversalsMapper2.type, Traversals, CTout],
        outMapper: shapeless.ops.hlist.Mapper.Aux[TraversalsMapper3.type, Traversals, Tout],
//        lub: shapeless.LUBConstraint[OutH, _ <: ClassType[_]],
        tupler: shapeless.ops.hlist.Tupler.Aux[Tout, Out])
      : Aux[Project[Traversals] :: HNil, ClassType[Nothing], Out, TupleType[Out]] =
      new Impl[Project[Traversals] :: HNil, ClassType[Nothing], Out, TupleType[Out]] {
        override def convert(hlist: Project[Traversals] :: HNil, value: ClassType[Nothing]): List[TupleType[Out]] = {
//          val (et :: containers) = mapper(hlist.head.by)
//          val outCt              = outCtMapper(et.asInstanceOf[ClassType[Any]] :: containers)

          List(
            TupleType[Out](outCtMapper(hlist.head.by).runtimeList
              .map(_.asInstanceOf[List[ClassType[Any]]].filter(_.iri.nonEmpty))))
//              .asInstanceOf[List[ClassType[innerKey.Out]]]
          //.asInstanceOf[TupleType[Out]])
        }
      }

    implicit def pathStep[L <: HList,
                          ET <: ClassType[_],
                          Segments <: HList,
                          Steps <: HList,
                          RSteps <: HList,
                          Containers <: HList](
        implicit
        flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
        reverse: Reverse.Aux[Steps, RSteps],
        f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
//        concat: shapeless.ops.hlist.Prepend.Aux[Containers, L, AllSegments],
        inner: StructureCalculator[Containers, ET])
      : Aux[Path[ET, Segments] :: HNil, ClassType[Nothing], List[inner.Out], CollectionType[List[inner.Out]]] =
      new Impl[Path[ET, Segments] :: HNil, ClassType[Nothing], List[inner.Out], CollectionType[List[inner.Out]]] {
        override def convert(hlist: Path[ET, Segments] :: HNil,
                             value: ClassType[Nothing]): List[CollectionType[List[inner.Out]]] = {
          val im = inner.convert(f(reverse(flat(hlist.head.by.segments))), hlist.head.by.et)
          List(
            ListType(
              im.asInstanceOf[List[ClassType[List[inner.Out]]]]
                .filter(_.iri.nonEmpty)).asInstanceOf[CollectionType[List[inner.Out]]])
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
