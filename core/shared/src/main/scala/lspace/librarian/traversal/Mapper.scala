package lspace.librarian.traversal

import lspace.librarian.task.{
  AsyncGroupedResult,
  AsyncListResult,
  AsyncOneResult,
  AsyncZeroOrOneResult,
  GroupedResult,
  Guide,
  ListResult,
  OneResult,
  Result,
  SyncGroupedResult,
  SyncListResult,
  SyncOneResult,
  SyncZeroOrOneResult,
  ZeroOrOneResult
}
import lspace.structure.{ClassType, Graph}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import shapeless.{::, HList, HNil}

sealed trait Mapper[G[_], Containers <: HList, T] {
  type F[_]
  type Out
  type FT <: Result[F, G, Out]

  def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT
}

object Mapper {
//  type Aux[G[_], Out, F0] = Mapper[G, Out] { type F = F0 }
  implicit def groupedStream[K, V, Container, Containers <: HList](implicit guide: Guide[Stream],
                                                                   ev: GroupedResult.IsGrouped[Container]) =
    new Mapper[Stream, Container :: Containers, (K, V)] {
      type F[_] = Coeval[_]
      type Out  = (K, V)
      type FT   = SyncGroupedResult[K, V]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncGroupedResult[K, V](traversal, graph)
    }

  implicit def streamh[T, Container, Containers <: HList](implicit guide: Guide[Stream],
                                                          ev: ListResult.IsList[Container]) =
    new Mapper[Stream, Container :: Containers, T] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncListResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncListResult[T](traversal, graph)
    }
  implicit def streamone[T, Container, Containers <: HList](implicit guide: Guide[Stream],
                                                            ev: OneResult.IsOne[Container]) =
    new Mapper[Stream, Container :: Containers, T] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncOneResult[T](traversal, graph)
    }
  implicit def streamzeroorone[T, Container, Containers <: HList](implicit guide: Guide[Stream],
                                                                  ev: ZeroOrOneResult.IsZeroOrOne[Container]) =
    new Mapper[Stream, Container :: Containers, T] {
      type F[_] = Coeval[_]
      type Out  = T
      type FT   = SyncZeroOrOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        SyncZeroOrOneResult[T](traversal, graph)
    }
  implicit def stream[T](implicit guide: Guide[Stream]) = new Mapper[Stream, HNil, T] {
    type F[_] = Coeval[_]
    type Out  = T
    type FT   = SyncListResult[T]

    def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
      SyncListResult[T](traversal, graph)
  }

  implicit def groupedObservable[K, V, Container, Containers <: HList](implicit guide: Guide[Observable],
                                                                       ev: GroupedResult.IsGrouped[Container]) =
    new Mapper[Observable, Container :: Containers, (K, V)] {
      type F[_] = Task[_]
      type Out  = (K, V)
      type FT   = AsyncGroupedResult[K, V]
      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncGroupedResult[K, V](traversal, graph)
    }
  implicit def observableh[T, Container, Containers <: HList](implicit guide: Guide[Observable],
                                                              ev: ListResult.IsList[Container]) =
    new Mapper[Observable, Container :: Containers, T] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncListResult[T]
      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncListResult[T](traversal, graph)
    }
  implicit def observableone[T, Container, Containers <: HList](implicit guide: Guide[Observable],
                                                                ev: OneResult.IsOne[Container]) =
    new Mapper[Observable, Container :: Containers, T] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncOneResult[T](traversal, graph)
    }
  implicit def observablezeroorone[T, Container, Containers <: HList](implicit guide: Guide[Observable],
                                                                      ev: ZeroOrOneResult.IsZeroOrOne[Container]) =
    new Mapper[Observable, Container :: Containers, T] {
      type F[_] = Task[_]
      type Out  = T
      type FT   = AsyncZeroOrOneResult[T]

      def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
        AsyncZeroOrOneResult[T](traversal, graph)
    }
  implicit def observable[T](implicit guide: Guide[Observable]) = new Mapper[Observable, HNil, T] {
    type F[_] = Task[_]
    type Out  = T
    type FT   = AsyncListResult[T]
    def apply(traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph): FT =
      AsyncListResult[T](traversal, graph)
  }
}
