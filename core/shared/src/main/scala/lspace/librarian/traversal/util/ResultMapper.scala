package lspace.librarian.traversal.util

import lspace.datatype._
import lspace.librarian.task._
import lspace.librarian.traversal.Traversal
import lspace.structure.{ClassType, Graph}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import shapeless.{<:!<, HList}

sealed trait TMapper[G[_], -ET <: ClassType[_], nET <: ClassType[_]] {
  type F[_]
  type Out
  type FT <: Result[F, G, Out]

  def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT
}

object TMapper {
//  type Aux[G[_], Out, F0] = Mapper[G, Out] { type F = F0 }
  implicit def groupedStream[K, V](implicit guide: Guide[Stream]) =
    new TMapper[Stream, TupleType[(K, V)], MapType[Map[K, V]]] {
      type F[_] = Coeval[_]
      type Out  = (K, V)
      type FT   = SyncGroupedResult[K, V]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncGroupedResult[K, V](traversal, graph)
    }

  implicit def streamh[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Stream]) =
    new TMapper[Stream, ET[T], ListType[List[T]]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncListResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncListResult[T](traversal, graph)
    }
  implicit def streamhs[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Stream]) =
    new TMapper[Stream, ET[T], SetType[Set[T]]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncListResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncListResult[T](traversal, graph)
    }
//  implicit def streamhg[K, V](implicit guide: Guide[Stream]) =
//    new Mapper[Stream, MapType[Map[K, V]], ListType[List[Map[K, V]]]] {
//      type F[_] = Coeval[_]
//      type Out  = Map[K, V]
//      type FT   = SyncListResult[Map[K, V]]
//
//      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
//        SyncListResult[Map[K, V]](traversal, graph)
//    }
  implicit def streamone[T, ET1[+Z] <: ClassType[Z], ET2[+Z] <: ClassType[Z]](implicit guide: Guide[Stream],
                                                                              ev0: ET2[_] <:!< MapType[_],
                                                                              ev1: ET2[_] <:!< ListType[_],
                                                                              ev2: ET2[_] <:!< SetType[_],
                                                                              ev3: ET2[_] <:!< OptionType[_]) =
    new TMapper[Stream, ET1[T], ET2[T]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncOneResult[T](traversal, graph)
    }
  implicit def streamzeroorone[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Stream]) =
    new TMapper[Stream, ET[T], OptionType[Option[T]]] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncZeroOrOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncZeroOrOneResult[T](traversal, graph)
    }

  implicit def groupedObservable[K, V](implicit guide: Guide[Observable]) =
    new TMapper[Observable, TupleType[(K, V)], MapType[Map[K, V]]] {
      type F[_] = Task[_]
      type Out  = (K, V)
      type FT   = AsyncGroupedResult[K, V]
      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncGroupedResult[K, V](traversal, graph)
    }

  implicit def observableh[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Observable]) =
    new TMapper[Observable, ET[T], ListType[List[T]]] {
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
  implicit def observablehs[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Observable]) =
    new TMapper[Observable, ET[T], SetType[Set[T]]] {
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
  implicit def observableone[T, ET1[+Z] <: ClassType[Z], ET2[+Z] <: ClassType[Z]](implicit guide: Guide[Observable],
                                                                                  ev0: ET2[_] <:!< MapType[_],
                                                                                  ev1: ET2[_] <:!< ListType[_],
                                                                                  ev2: ET2[_] <:!< SetType[_],
                                                                                  ev3: ET2[_] <:!< OptionType[_]) =
    new TMapper[Observable, ET1[T], ET2[T]] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncOneResult[T](traversal, graph)
    }
  implicit def observablezeroorone[T, ET[+Z] <: ClassType[Z]](implicit guide: Guide[Observable]) =
    new TMapper[Observable, ET[T], OptionType[Option[T]]] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncZeroOrOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncZeroOrOneResult[T](traversal, graph)
    }
}
