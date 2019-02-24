package lspace.librarian.task

import lspace.librarian.traversal.Segment
import lspace.structure.Graph
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import shapeless.HList

trait FTraversal[+F[_], O[_], Out] {
  def segments: List[Segment[HList]]
  def graph: Graph

  def headF: F[Out]
  def headOptionF: F[Option[Out]]
  def toListF: F[List[Out]]
  def toSetF: F[Set[Out]]
  def map[T](f: Out => T): O[T]
  def flatMap[T](f: Out => O[T]): O[T]

  def apply(): O[Out]
}
trait GroupedTraversal[F[_], O[_], OutK, OutV] extends FTraversal[F, O, (OutK, OutV)] {
  def toMapF: F[Map[OutK, OutV]]
}

object AsyncGroupedTraversal {
  def apply[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncGroupedTraversal[OutK, OutV] =
    new AsyncGroupedTraversal[OutK, OutV](segments, graph)
}
class AsyncGroupedTraversal[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
    extends AsyncTraversal[(OutK, OutV)](segments, graph)(guide)
    with GroupedTraversal[Task, Observable, OutK, OutV] {

  def toMapF: Task[Map[OutK, OutV]] = guide.buildTraversal[(OutK, OutV)](segments)(graph).toListL.map(_.toMap)
}

//case class TraversalAsyncTask[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
object AsyncTraversal {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable]): AsyncTraversal[Out] =
    new AsyncTraversal(segments, graph)
}
class AsyncTraversal[Out](val segments: List[Segment[HList]], val graph: Graph)(implicit guide: Guide[Observable])
    extends FTraversal[Task, Observable, Out] {

  def apply(): Observable[Out] =
    guide.buildTraversal[Out](segments)(graph)

  def headF: Task[Out] =
    guide.buildTraversal[Out](segments)(graph).headL
  def headOptionF: Task[Option[Out]] =
    guide.buildTraversal[Out](segments)(graph).headOptionL
  def toListF: Task[List[Out]] =
    guide.buildTraversal[Out](segments)(graph).toListL
  def toSetF: Task[Set[Out]]                             = guide.buildTraversal[Out](segments)(graph).toListL.map(_.toSet)
  def map[T](f: Out => T): Observable[T]                 = guide.buildTraversal[Out](segments)(graph).map(f)
  def flatMap[T](f: Out => Observable[T]): Observable[T] = guide.buildTraversal[Out](segments)(graph).flatMap(f)
}

object SyncGroupedTraversal {
  def apply[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Stream]): SyncGroupedTraversal[OutK, OutV] =
    new SyncGroupedTraversal[OutK, OutV](segments, graph)
}
class SyncGroupedTraversal[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
    extends SyncTraversal[(OutK, OutV)](segments, graph)(guide)
    with GroupedTraversal[Coeval, Stream, OutK, OutV] {

  def toMapF: Coeval[Map[OutK, OutV]] = Coeval(guide.buildTraversal[(OutK, OutV)](segments)(graph).toMap)
  def toMap: Map[OutK, OutV]          = toMapF.value()
}
object SyncTraversal {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream]): SyncTraversal[Out] =
    new SyncTraversal(segments, graph)
}
//case class TraversalSyncTask[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
class SyncTraversal[Out](val segments: List[Segment[HList]], val graph: Graph)(implicit guide: Guide[Stream])
    extends FTraversal[Coeval, Stream, Out] {

  def apply(): Stream[Out] =
    guide.buildTraversal[Out](segments)(graph)

  def headF: Coeval[Out] =
    Coeval(guide.buildTraversal[Out](segments)(graph).head)
  def head: Out = headF.value()
  def headOptionF: Coeval[Option[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).headOption)
  def headOption: Option[Out] = headOptionF.value()
  def toListF: Coeval[List[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).toList)
  def toList: List[Out] = toListF.value()
  def toSetF: Coeval[Set[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).toSet)
  def toSet: Set[Out]                            = toSetF.value()
  def map[T](f: Out => T): Stream[T]             = guide.buildTraversal[Out](segments)(graph).map(f)
  def flatMap[T](f: Out => Stream[T]): Stream[T] = guide.buildTraversal[Out](segments)(graph).flatMap(f)
}
