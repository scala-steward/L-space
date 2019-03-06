package lspace.librarian.task

import lspace.librarian.traversal.Segment
import lspace.librarian.traversal
import lspace.librarian.traversal.step
import lspace.structure.Graph
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import shapeless.HList

object Result {
  trait Is[T]
  object Is {
    implicit object default extends Is[Any]
  }
}
trait Result[+F[_], O[_], Out] {
  def segments: List[Segment[HList]]
  def graph: Graph

  def map[T](f: Out => T): O[T]
  def flatMap[T](f: Out => O[T]): O[T]

  def apply(): O[Out]
}
object OneResult {
  sealed trait IsOne[T] extends Result.Is[T]
  object IsOne {
    implicit object Count extends IsOne[step.Count]
  }
}
trait OneResult[+F[_], O[_], Out] extends Result[F, O, Out] {
  def headF: F[Out]
}
object ZeroOrOneResult {
  sealed trait IsZeroOrOne[T] extends Result.Is[T]
  object IsZeroOrOne {
    implicit object Head extends IsZeroOrOne[step.Head]
    implicit object Last extends IsZeroOrOne[step.Last]
    implicit object Min  extends IsZeroOrOne[step.Min]
    implicit object Max  extends IsZeroOrOne[step.Max]
    implicit object Mean extends IsZeroOrOne[step.Mean]
  }
}
trait ZeroOrOneResult[+F[_], O[_], Out] extends OneResult[F, O, Out] {
  def headOptionF: F[Option[Out]]
}

object ListResult {
  sealed trait IsList[T] extends Result.Is[T]
  object IsList {
    implicit def Project[T <: step.Project[_]]   = new IsList[T] {}
    implicit def Path[T <: step.Path[_, _]]      = new IsList[T] {}
    implicit def MapStep[T <: traversal.MapStep] = new IsList[T] {}
  }
}
trait ListResult[+F[_], O[_], Out] extends ZeroOrOneResult[F, O, Out] {
  def lastF: F[Out]
  def lastOptionF: F[Option[Out]]

  def toListF: F[List[Out]]
  def toSetF: F[Set[Out]]
}
object GroupedResult {
  sealed trait IsGrouped[T] extends Result.Is[T]
  object IsGrouped {
    implicit def Group[T <: step.Group[_, _]] = new IsGrouped[T] {}
  }
}
trait GroupedResult[+F[_], O[_], OutK, OutV] extends ListResult[F, O, (OutK, OutV)] {
  def toMapF: F[Map[OutK, OutV]]
}

object AsyncGroupedResult {
  def apply[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncGroupedResult[OutK, OutV] =
    new AsyncGroupedResult[OutK, OutV](segments, graph)
}
class AsyncGroupedResult[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
    extends AsyncListResult[(OutK, OutV)](segments, graph)(guide)
    with GroupedResult[Task, Observable, OutK, OutV] {

  def toMapF: Task[Map[OutK, OutV]] = guide.buildTraversal[(OutK, OutV)](segments)(graph).toListL.map(_.toMap)
}

object AsyncListResult {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncListResult[Out] =
    new AsyncListResult(segments, graph)
}
class AsyncListResult[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
    extends AsyncZeroOrOneResult[Out](segments, graph)(guide)
    with ListResult[Task, Observable, Out] {

  def lastF: Task[Out] =
    guide.buildTraversal[Out](segments)(graph).lastL
  def lastOptionF: Task[Option[Out]] =
    guide.buildTraversal[Out](segments)(graph).lastOptionL

  def toListF: Task[List[Out]] =
    guide.buildTraversal[Out](segments)(graph).toListL
  def toSetF: Task[Set[Out]] = guide.buildTraversal[Out](segments)(graph).toListL.map(_.toSet)
}
//case class TraversalAsyncTask[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
object AsyncZeroOrOneResult {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncZeroOrOneResult[Out] =
    new AsyncZeroOrOneResult(segments, graph)
}
class AsyncZeroOrOneResult[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable])
    extends AsyncOneResult[Out](segments, graph)(guide)
    with ZeroOrOneResult[Task, Observable, Out] {

  def headOptionF: Task[Option[Out]] =
    guide.buildTraversal[Out](segments)(graph).headOptionL
}

object AsyncOneResult {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Observable]): AsyncOneResult[Out] =
    new AsyncOneResult(segments, graph)
}
class AsyncOneResult[Out](val segments: List[Segment[HList]], val graph: Graph)(implicit guide: Guide[Observable])
    extends OneResult[Task, Observable, Out] {

  def apply(): Observable[Out] =
    guide.buildTraversal[Out](segments)(graph)

  def headF: Task[Out] =
    guide.buildTraversal[Out](segments)(graph).headL

  def map[T](f: Out => T): Observable[T]                 = guide.buildTraversal[Out](segments)(graph).map(f)
  def flatMap[T](f: Out => Observable[T]): Observable[T] = guide.buildTraversal[Out](segments)(graph).flatMap(f)
}

object SyncGroupedResult {
  def apply[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Stream]): SyncGroupedResult[OutK, OutV] =
    new SyncGroupedResult[OutK, OutV](segments, graph)
}
class SyncGroupedResult[OutK, OutV](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
    extends SyncListResult[(OutK, OutV)](segments, graph)(guide)
    with GroupedResult[Coeval, Stream, OutK, OutV] {

  def toMapF: Coeval[Map[OutK, OutV]] = Coeval(guide.buildTraversal[(OutK, OutV)](segments)(graph).toMap)
  def toMap: Map[OutK, OutV]          = toMapF.value()
}
object SyncListResult {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream]): SyncListResult[Out] =
    new SyncListResult(segments, graph)
}
class SyncListResult[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
    extends SyncZeroOrOneResult[Out](segments, graph)(guide)
    with ListResult[Coeval, Stream, Out] {

  def lastF: Coeval[Out] =
    Coeval(guide.buildTraversal[Out](segments)(graph).last)
  def last: Out = lastF.value()
  def lastOptionF: Coeval[Option[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).lastOption)
  def lastOption: Option[Out] = lastOptionF.value()

  def toListF: Coeval[List[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).toList)
  def toList: List[Out] = toListF.value()
  def toSetF: Coeval[Set[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).toSet)
  def toSet: Set[Out] = toSetF.value()
}

object SyncZeroOrOneResult {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(
      implicit guide: Guide[Stream]): SyncZeroOrOneResult[Out] =
    new SyncZeroOrOneResult(segments, graph)
}
//case class TraversalSyncTask[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
class SyncZeroOrOneResult[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream])
    extends SyncOneResult[Out](segments, graph)(guide)
    with ZeroOrOneResult[Coeval, Stream, Out] {

  def headOptionF: Coeval[Option[Out]] =
    Coeval(guide.buildTraversal[Out](segments)(graph).headOption)
  def headOption: Option[Out] = headOptionF.value()
}

object SyncOneResult {
  def apply[Out](segments: List[Segment[HList]], graph: Graph)(implicit guide: Guide[Stream]): SyncOneResult[Out] =
    new SyncOneResult(segments, graph)
}
class SyncOneResult[Out](val segments: List[Segment[HList]], val graph: Graph)(implicit guide: Guide[Stream])
    extends OneResult[Coeval, Stream, Out] {

  def apply(): Stream[Out] =
    guide.buildTraversal[Out](segments)(graph)

  def headF: Coeval[Out] =
    Coeval(guide.buildTraversal[Out](segments)(graph).head)
  def head: Out = headF.value()

  def map[T](f: Out => T): Stream[T]             = guide.buildTraversal[Out](segments)(graph).map(f)
  def flatMap[T](f: Out => Stream[T]): Stream[T] = guide.buildTraversal[Out](segments)(graph).flatMap(f)
}
