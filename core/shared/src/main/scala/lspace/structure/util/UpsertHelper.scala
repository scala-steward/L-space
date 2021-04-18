package lspace.structure.util

import java.util.concurrent.ConcurrentHashMap

import lspace.structure.{Edge, Graph, Node, Value}
import monix.eval.Task
import monix.reactive.Observable

import scala.util.Try

object UpsertHelper {
  def apply(): UpsertHelper = new UpsertHelper()
}
class UpsertHelper() {
  import scala.jdk.CollectionConverters._
  private val oldIdNewNodeMap: scala.collection.concurrent.Map[Long, Node] =
    new ConcurrentHashMap[Long, Node]().asScala
  private val oldIdNewEdgeMap: scala.collection.concurrent.Map[Long, Edge[_, _]] =
    new ConcurrentHashMap[Long, Edge[_, _]]().asScala
  private val edgesToRetry: scala.collection.concurrent.Map[Long, Edge[_, _]] =
    new ConcurrentHashMap[Long, Edge[_, _]]().asScala
  private val oldIdNewValueMap: scala.collection.concurrent.Map[Long, Value[_]] =
    new ConcurrentHashMap[Long, Value[_]]().asScala

  def createNode(id: Long, nodeTask: Task[Node]): Task[Node] = synchronized {
    oldIdNewNodeMap.get(id).map(Task.now).getOrElse {
      nodeTask.map { node =>
        oldIdNewNodeMap.update(id, node)
        node
      }
    }
  }

  def createEdge[S, E](id: Long, edgeTask: Task[Edge[S, E]]): Task[Edge[S, E]] = synchronized {
    oldIdNewEdgeMap.get(id).map(Task.now).getOrElse {
      edgeTask.map { edge =>
        oldIdNewEdgeMap.update(id, edge)
        edge
      }
    }.asInstanceOf[Task[Edge[S, E]]]
  }

  def createValue[V](id: Long, valueTask: Task[Value[V]]): Task[Value[V]] = synchronized {
    oldIdNewValueMap.get(id).map(Task.now).getOrElse {
      valueTask.map { value =>
        oldIdNewValueMap.update(id, value)
        value
      }
    }.asInstanceOf[Task[Value[V]]]
  }

  def retryEdges(implicit graph: Graph): Task[Boolean] = Task.defer {
    Observable
      .fromIterable(edgesToRetry.values)
      .mapEval { edge =>
        mergeEdge(edge)
          .doOnFinish {
            case None => Task(edgesToRetry -= edge.id)
            case _ => Task.unit
          }
          .onErrorHandle(_ => ())
      }
      .completedL
      .map { _ =>
        edgesToRetry.isEmpty
      }
  }
  def mergeEdge(edge: Edge[_, _])(implicit graph: Graph): Task[Unit] = Task.defer {
    (for {
      from <- Try(edge.from match {
        case (resource: Node) =>
          oldIdNewNodeMap(resource.id)
        case (resource: Edge[_, _]) =>
          oldIdNewEdgeMap(resource.id)
        case (resource: Value[_]) =>
          oldIdNewValueMap(resource.id)
      }).toOption
      to <- Try(edge.to match {
        case (resource: Node) =>
          oldIdNewNodeMap(resource.id)
        case (resource: Edge[_, _]) =>
          oldIdNewEdgeMap(resource.id)
        case (resource: Value[_]) =>
          oldIdNewValueMap(resource.id)
      }).toOption
    } yield {
      createEdge(edge.id, graph.edges.create(
        from,
        edge.key,
        to
      ))
    }).getOrElse {
      Task.raiseError {
        edgesToRetry += (edge.id -> edge)
        new Exception("could not merge yet")
      }
    }.void
  }
}
