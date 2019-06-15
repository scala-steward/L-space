package lspace.structure

import lspace.datatype.DataType
import lspace.librarian.traversal.{Step, Traversal}
import lspace.structure.index.Indexes
import lspace.structure.util.IdProvider
import monix.eval.Task
import shapeless.HList

trait IndexGraph extends Graph {
  def graph: Graph
  def ns: NameSpaceGraph = graph.ns

  lazy val idProvider: IdProvider = graph.idProvider

//  protected def `@patternIndex`: Index
//  protected def `@typeIndex`: Index

  protected[lspace] def indexes: Indexes

  lazy val init: Task[Unit] = Task.unit

  implicit def stepListToTraversal(steps: List[Step]): Traversal[ClassType[Any], ClassType[Any], _ <: HList] =
    Traversal(steps.toVector)
//  def findIndex(traversal: UntypedTraversal): List[Node] = {
//    stepListToTraversal(
//      g.N
//        .hasLabel(Index.ontology)
//        .steps ::: traversal.segments.zipWithIndex.foldLeft(List[Step]()) {
//        case (stepList, (segment, count)) =>
//          val steps = segment.stepsList.collect { case step: Has => step }
//          import _step._
//          Where(
//            Out(Set(Index.keys.traversal.property)) :: Out(Set(Traversal.keys.segment.property)) ::
//              Range(count, count) :: Out(Set(Segment.keys.step.property)) :: Has(
//              Has.keys.key.property,
//              Some(P.||(steps.map(_.key).map(P.eqv(_)): _*))) :: Nil) :: stepList
//      }).untyped.toTyped.toList.asInstanceOf[List[Node]]
//  }

//  def find[T](predicates: List[P[T]], property: Property): List[Resource[T]] = {
//    getIndex(Shape(property)).toList
//      .flatMap(_.find(predicates, property))
//  }
//
//  def find(values: Vector[Map[Property, List[P[_]]]]): List[Vector[Resource[_]]] = {
//    getIndex(values.map(_.keySet).map(Shape(_))).toList.flatMap(_.find(values))
//  }

  override protected[lspace] def deleteNode(node: _Node): Task[Unit] = {
    //    `@typeIndex`.delete()
    super.deleteNode(node)
  }

  abstract override protected[lspace] def createEdge[S, E](id: Long,
                                                           from: _Resource[S],
                                                           key: Property,
                                                           to: _Resource[E]): Task[GEdge[S, E]] =
    for {
      edge <- super.createEdge(id, from, key, to)
      u    <- storeEdge(edge.asInstanceOf[_Edge[_, _]])
    } yield edge

  override protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] =
    super.deleteEdge(edge)

  abstract override protected[lspace] def createValue[T](_id: Long, _value: T, dt: DataType[T]): Task[GValue[T]] =
    for {
      value <- super.createValue(_id, _value, dt)
//      u     <- storeValue(value /*.asInstanceOf[_Value[_]]*/ )
    } yield value

  override protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] =
    super.deleteValue(value)

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
    } yield ()

}
