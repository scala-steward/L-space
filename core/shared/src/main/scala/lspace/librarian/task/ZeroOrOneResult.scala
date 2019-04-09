package lspace.librarian.task

import lspace.librarian.traversal.{step, Segment, Traversal}
import lspace.librarian.traversal
import lspace.structure.{ClassType, Graph}
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
  def traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]
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
    implicit def Group[T <: step.Group[_, _, _, _]] = new IsGrouped[T] {}
  }
}
trait GroupedResult[+F[_], O[_], OutK, OutV] extends ListResult[F, O, (OutK, OutV)] {
  def toMapF: F[Map[OutK, OutV]]
}

object AsyncGroupedResult {
  def apply[OutK, OutV](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncGroupedResult[OutK, OutV] =
    new AsyncGroupedResult[OutK, OutV](traversal, graph)
}
class AsyncGroupedResult[OutK, OutV](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
                                     graph: Graph)(implicit guide: Guide[Observable])
    extends AsyncListResult[(OutK, OutV)](traversal, graph)(guide)
    with GroupedResult[Task, Observable, OutK, OutV] {

  def toMapF: Task[Map[OutK, OutV]] = guide.executeTraversal[(OutK, OutV)](traversal)(graph).toListL.map(_.toMap)
}

object AsyncListResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncListResult[Out] =
    new AsyncListResult(traversal, graph)
}
class AsyncListResult[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
    implicit guide: Guide[Observable])
    extends AsyncZeroOrOneResult[Out](traversal, graph)(guide)
    with ListResult[Task, Observable, Out] {

  def lastF: Task[Out] =
    guide.executeTraversal[Out](traversal)(graph).lastL
  def lastOptionF: Task[Option[Out]] =
    guide.executeTraversal[Out](traversal)(graph).lastOptionL

  def toListF: Task[List[Out]] =
    guide.executeTraversal[Out](traversal)(graph).toListL
  def toSetF: Task[Set[Out]] = guide.executeTraversal[Out](traversal)(graph).toListL.map(_.toSet)
}
//case class TraversalAsyncTask[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit guide: Guide[Observable])
object AsyncZeroOrOneResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncZeroOrOneResult[Out] =
    new AsyncZeroOrOneResult(traversal, graph)
}
class AsyncZeroOrOneResult[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
                                graph: Graph)(implicit guide: Guide[Observable])
    extends AsyncOneResult[Out](traversal, graph)(guide)
    with ZeroOrOneResult[Task, Observable, Out] {

  def headOptionF: Task[Option[Out]] =
    guide.executeTraversal[Out](traversal)(graph).headOptionL
}

object AsyncOneResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Observable]): AsyncOneResult[Out] =
    new AsyncOneResult(traversal, graph)
}
class AsyncOneResult[Out](val traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
                          val graph: Graph)(implicit guide: Guide[Observable])
    extends OneResult[Task, Observable, Out] {

  def apply(): Observable[Out] =
    guide.executeTraversal[Out](traversal)(graph)

  def headF: Task[Out] =
    guide.executeTraversal[Out](traversal)(graph).headL

  def map[T](f: Out => T): Observable[T]                 = guide.executeTraversal[Out](traversal)(graph).map(f)
  def flatMap[T](f: Out => Observable[T]): Observable[T] = guide.executeTraversal[Out](traversal)(graph).flatMap(f)
}

object SyncGroupedResult {
  def apply[OutK, OutV](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Stream]): SyncGroupedResult[OutK, OutV] =
    new SyncGroupedResult[OutK, OutV](traversal, graph)
}
class SyncGroupedResult[OutK, OutV](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
                                    graph: Graph)(implicit guide: Guide[Stream])
    extends SyncListResult[(OutK, OutV)](traversal, graph)(guide)
    with GroupedResult[Coeval, Stream, OutK, OutV] {

  def toMapF: Coeval[Map[OutK, OutV]] = Coeval(guide.executeTraversal[(OutK, OutV)](traversal)(graph).toMap)
  def toMap: Map[OutK, OutV]          = toMapF.value()
}
object SyncListResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Stream]): SyncListResult[Out] =
    new SyncListResult(traversal, graph)
}
class SyncListResult[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
    implicit guide: Guide[Stream])
    extends SyncZeroOrOneResult[Out](traversal, graph)(guide)
    with ListResult[Coeval, Stream, Out] {

  def lastF: Coeval[Out] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).last)
  def last: Out = lastF.value()
  def lastOptionF: Coeval[Option[Out]] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).lastOption)
  def lastOption: Option[Out] = lastOptionF.value()

  def toListF: Coeval[List[Out]] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).toList)
  def toList: List[Out] = toListF.value()
  def toSetF: Coeval[Set[Out]] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).toSet)
  def toSet: Set[Out] = toSetF.value()
}

object SyncZeroOrOneResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Stream]): SyncZeroOrOneResult[Out] =
    new SyncZeroOrOneResult(traversal, graph)
}
//case class TraversalSyncTask[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit guide: Guide[Stream])
class SyncZeroOrOneResult[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
    implicit guide: Guide[Stream])
    extends SyncOneResult[Out](traversal, graph)(guide)
    with ZeroOrOneResult[Coeval, Stream, Out] {

  def headOptionF: Coeval[Option[Out]] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).headOption)
  def headOption: Option[Out] = headOptionF.value()
}

object SyncOneResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
      implicit guide: Guide[Stream]): SyncOneResult[Out] =
    new SyncOneResult(traversal, graph)
}
class SyncOneResult[Out](val traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
                         val graph: Graph)(implicit guide: Guide[Stream])
    extends OneResult[Coeval, Stream, Out] {

  def apply(): Stream[Out] =
    guide.executeTraversal[Out](traversal)(graph)

  def headF: Coeval[Out] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).head)
  def head: Out = headF.value()

  def map[T](f: Out => T): Stream[T]             = guide.executeTraversal[Out](traversal)(graph).map(f)
  def flatMap[T](f: Out => Stream[T]): Stream[T] = guide.executeTraversal[Out](traversal)(graph).flatMap(f)
}
