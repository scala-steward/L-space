package lspace.librarian.traversal.util

import lspace.datatype._
import lspace.librarian.task._
import lspace.librarian.traversal.Traversal
import lspace.structure.{ClassType, Graph}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import shapeless.{<:!<, HList}

import scala.annotation.unused

sealed trait ResultMapper[G[_], -ET <: ClassType[_], nET <: ClassType[_]] {
  type F[_]
  type Out
  type FT <: Result[F, G, Out]

  def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT
}

object ResultMapper {
  type Aux[G[_], -ET <: ClassType[_], nET <: ClassType[_], /*Out0, */ FT0] = ResultMapper[G, ET, nET] {
//    type Out = Out0
    type FT = FT0
  }
  implicit def groupedLazyList[K, V](implicit
    guide: Guide[LazyList]
  ): ResultMapper.Aux[LazyList, TupleType[(K, V)], MapType[Map[K, V]], SyncGroupedResult[K, V]] =
    new ResultMapper[LazyList, TupleType[(K, V)], MapType[Map[K, V]]] {
      type F[_] = Coeval[_]
      type Out  = (K, V)
      type FT   = SyncGroupedResult[K, V]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncGroupedResult[K, V](traversal, graph)
    }

  implicit def lazylisth[T, ET[+Z] <: ClassType[Z]](implicit
    guide: Guide[LazyList]
  ): ResultMapper.Aux[LazyList, ET[T], ListType[List[T]], SyncListResult[T]] =
    new ResultMapper[LazyList, ET[T], ListType[List[T]]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncListResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncListResult[T](traversal, graph)
    }
  implicit def lazylisths[T, ET[+Z] <: ClassType[Z]](implicit
    guide: Guide[LazyList]
  ): ResultMapper.Aux[LazyList, ET[T], SetType[Set[T]], SyncListResult[T]] =
    new ResultMapper[LazyList, ET[T], SetType[Set[T]]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncListResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncListResult[T](traversal, graph)
    }
//  implicit def lazylisthg[K, V](implicit guide: Guide[LazyList]) =
//    new Mapper[LazyList, MapType[Map[K, V]], ListType[List[Map[K, V]]]] {
//      type F[_] = Coeval[_]
//      type Out  = Map[K, V]
//      type FT   = SyncListResult[Map[K, V]]
//
//      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
//        SyncListResult[Map[K, V]](traversal, graph)
//    }
  implicit def lazylistone[T, ET1[+Z] <: ClassType[Z], ET2[+Z] <: ClassType[Z]](implicit
    guide: Guide[LazyList],
    ev0: ET2[_] <:!< MapType[_],
    ev1: ET2[_] <:!< ListType[_],
    ev2: ET2[_] <:!< SetType[_],
    ev3: ET2[_] <:!< OptionType[_]
  ): ResultMapper.Aux[LazyList, ET1[T], ET2[T], SyncOneResult[T]] =
    new ResultMapper[LazyList, ET1[T], ET2[T]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncOneResult[T](traversal, graph)
    }
  implicit def lazylistzeroorone[T, ET[+Z] <: ClassType[Z]](implicit
    guide: Guide[LazyList]
  ): ResultMapper.Aux[LazyList, ET[T], OptionType[Option[T]], SyncZeroOrOneResult[T]] =
    new ResultMapper[LazyList, ET[T], OptionType[Option[T]]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncZeroOrOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncZeroOrOneResult[T](traversal, graph)
    }

  implicit def groupedObservable[K, V](implicit
    guide: Guide[Observable]
  ): ResultMapper.Aux[Observable, TupleType[(K, V)], MapType[Map[K, V]], AsyncGroupedResult[K, V]] =
    new ResultMapper[Observable, TupleType[(K, V)], MapType[Map[K, V]]] {
      type F[_] = Task[_]
      type Out  = (K, V)
      type FT   = AsyncGroupedResult[K, V]
      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncGroupedResult[K, V](traversal, graph)
    }

  implicit def observableh[T, ET[+Z] <: ClassType[Z]](implicit
    guide: Guide[Observable]
  ): ResultMapper.Aux[Observable, ET[T], ListType[List[T]], AsyncListResult[T]] =
    new ResultMapper[Observable, ET[T], ListType[List[T]]] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncListResult[T]
      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncListResult[T](traversal, graph)
    }
//  implicit def observablehh[T](implicit guide: Guide[Observable]) =
//    new Mapper[Observable, ListType[T], ListType[List[T]]] {
//      type F[_] = Task[_]
//      type Out  = List[T]
//      type FT   = AsyncListResult[List[T]]
//      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
//        AsyncListResult[List[T]](traversal, graph)
//    }
  implicit def observablehs[T, ET[+Z] <: ClassType[Z]](implicit
    guide: Guide[Observable]
  ): ResultMapper.Aux[Observable, ET[T], SetType[Set[T]], AsyncListResult[T]] =
    new ResultMapper[Observable, ET[T], SetType[Set[T]]] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncListResult[T]
      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncListResult[T](traversal, graph)
    }
//  implicit def observablehg[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Observable]) =
//    new Mapper[Observable, ET[T], ListType[List[T]]] {
//      type F[_] = Task[_]
//      type Out  = T
//      type FT   = AsyncListResult[T]
//      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
//        AsyncListResult[T](traversal, graph)
//    }
  implicit def observableone[T, ET1[+Z] <: ClassType[Z], ET2[+Z] <: ClassType[Z]](implicit
    guide: Guide[Observable],
    @unused ev0: ET2[_] <:!< MapType[_],
    @unused ev1: ET2[_] <:!< ListType[_],
    @unused ev2: ET2[_] <:!< SetType[_],
    @unused ev3: ET2[_] <:!< OptionType[_]
  ): ResultMapper.Aux[Observable, ET1[T], ET2[T], AsyncOneResult[T]] =
    new ResultMapper[Observable, ET1[T], ET2[T]] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncOneResult[T](traversal, graph)
    }
  implicit def observablezeroorone[T, ET[+Z] <: ClassType[Z]](implicit
    guide: Guide[Observable]
  ): ResultMapper.Aux[Observable, ET[T], OptionType[Option[T]], AsyncZeroOrOneResult[T]] =
    new ResultMapper[Observable, ET[T], OptionType[Option[T]]] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncZeroOrOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncZeroOrOneResult[T](traversal, graph)
    }
}
