package lspace.librarian.task

import lspace.librarian.traversal.Traversal
import lspace.structure.{ClassType, Graph}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import shapeless.HList

trait Result[+F[_], O[_], Out] {
  def traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]
  def graph: Graph

  def map[T](f: Out => T): O[T]
  def flatMap[T](f: Out => O[T]): O[T]

  def apply(): O[Out]
}

trait OneResult[+F[_], O[_], Out] extends Result[F, O, Out] {
  def headF: F[Out]
}

trait ZeroOrOneResult[+F[_], O[_], Out] extends OneResult[F, O, Out] {
  def headOptionF: F[Option[Out]]
}

trait ListResult[+F[_], O[_], Out] extends ZeroOrOneResult[F, O, Out] {
  def lastF: F[Out]
  def lastOptionF: F[Option[Out]]

  def toListF: F[List[Out]]
  def toSetF: F[Set[Out]]
}

trait GroupedResult[+F[_], O[_], OutK, OutV] extends ListResult[F, O, (OutK, OutV)] {
  def toMapF: F[Map[OutK, OutV]]
}

object AsyncGroupedResult {
  def apply[OutK, OutV](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
    implicit guide: Guide[Observable]
  ): AsyncGroupedResult[OutK, OutV] =
    new AsyncGroupedResult[OutK, OutV](traversal, graph)
}
class AsyncGroupedResult[OutK, OutV](
  traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
  graph: Graph
)(implicit guide: Guide[Observable])
    extends AsyncListResult[(OutK, OutV)](traversal, graph)(guide)
    with GroupedResult[Task, Observable, OutK, OutV] {

  def toMapF: Task[Map[OutK, OutV]] = guide.executeTraversal[(OutK, OutV)](traversal)(graph).toListL.map(_.toMap)
}

object AsyncListResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit
    guide: Guide[Observable]
  ): AsyncListResult[Out] =
    new AsyncListResult(traversal, graph)
}
class AsyncListResult[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
  implicit guide: Guide[Observable]
) extends AsyncZeroOrOneResult[Out](traversal, graph)(guide)
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
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit
    guide: Guide[Observable]
  ): AsyncZeroOrOneResult[Out] =
    new AsyncZeroOrOneResult(traversal, graph)
}
class AsyncZeroOrOneResult[Out](
  traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
  graph: Graph
)(implicit guide: Guide[Observable])
    extends AsyncOneResult[Out](traversal, graph)(guide)
    with ZeroOrOneResult[Task, Observable, Out] {

  def headOptionF: Task[Option[Out]] =
    guide.executeTraversal[Out](traversal)(graph).headOptionL
}

object AsyncOneResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit
    guide: Guide[Observable]
  ): AsyncOneResult[Out] =
    new AsyncOneResult(traversal, graph)
}
class AsyncOneResult[Out](
  val traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
  val graph: Graph
)(implicit guide: Guide[Observable])
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
    implicit guide: Guide[LazyList]
  ): SyncGroupedResult[OutK, OutV] =
    new SyncGroupedResult[OutK, OutV](traversal, graph)
}
class SyncGroupedResult[OutK, OutV](
  traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
  graph: Graph
)(implicit guide: Guide[LazyList])
    extends SyncListResult[(OutK, OutV)](traversal, graph)(guide)
    with GroupedResult[Coeval, LazyList, OutK, OutV] {

  def toMapF: Coeval[Map[OutK, OutV]] = Coeval(guide.executeTraversal[(OutK, OutV)](traversal)(graph).toMap)
  def toMap: Map[OutK, OutV]          = toMapF.value()
}
object SyncListResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit
    guide: Guide[LazyList]
  ): SyncListResult[Out] =
    new SyncListResult(traversal, graph)
}
class SyncListResult[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(
  implicit guide: Guide[LazyList]
) extends SyncZeroOrOneResult[Out](traversal, graph)(guide)
    with ListResult[Coeval, LazyList, Out] {

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
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit
    guide: Guide[LazyList]
  ): SyncZeroOrOneResult[Out] =
    new SyncZeroOrOneResult(traversal, graph)
}
//case class TraversalSyncTask[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit guide: Guide[LazyList])
class SyncZeroOrOneResult[Out](
  traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
  graph: Graph
)(implicit guide: Guide[LazyList])
    extends SyncOneResult[Out](traversal, graph)(guide)
    with ZeroOrOneResult[Coeval, LazyList, Out] {

  def headOptionF: Coeval[Option[Out]] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).headOption)
  def headOption: Option[Out] = headOptionF.value()
}

object SyncOneResult {
  def apply[Out](traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList], graph: Graph)(implicit
    guide: Guide[LazyList]
  ): SyncOneResult[Out] =
    new SyncOneResult(traversal, graph)
}
class SyncOneResult[Out](
  val traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
  val graph: Graph
)(implicit guide: Guide[LazyList])
    extends OneResult[Coeval, LazyList, Out] {

  def apply(): LazyList[Out] =
    guide.executeTraversal[Out](traversal)(graph)

  def headF: Coeval[Out] =
    Coeval(guide.executeTraversal[Out](traversal)(graph).head)
  def head: Out = headF.value()

  def map[T](f: Out => T): LazyList[T]               = guide.executeTraversal[Out](traversal)(graph).map(f)
  def flatMap[T](f: Out => LazyList[T]): LazyList[T] = guide.executeTraversal[Out](traversal)(graph).flatMap(f)
}
