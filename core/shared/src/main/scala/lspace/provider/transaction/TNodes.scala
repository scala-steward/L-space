package lspace.provider.transaction

import java.util.concurrent.ConcurrentHashMap

import lspace.structure.{Node, Nodes}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.JavaConverters._

abstract class TNodes[G <: Transaction](override val graph: G) extends Nodes(graph) {
  import graph._

  val added: scala.collection.concurrent.Map[Long, GNode] = new ConcurrentHashMap[Long, GNode]().asScala
  val deleted: scala.collection.concurrent.Map[Long, parent.GNode] =
    new ConcurrentHashMap[Long, parent.GNode]().asScala

  override def apply(): Observable[Node] = {
    val tnodes = super.apply()
    val idSet: scala.collection.concurrent.Map[Long, Node] =
      new ConcurrentHashMap[Long, Node]().asScala
    tnodes.map { node =>
      idSet += node.id -> node; node
    } ++ parent.nodes().filter(n => !idSet.contains(n.id))
  }

  override def hasIri(iris: List[String]): Observable[Node] = {
    val fromTransaction = super.hasIri(iris)
    val fromParent =
      parent.nodes
        .hasIri(iris)
        .asInstanceOf[Observable[parent._Node]]
        .mapEval(_TNode(_).task)
        .filter(n => !deleted.contains(n.id))
    val idSet: scala.collection.concurrent.Map[Long, Node] =
      new ConcurrentHashMap[Long, Node]().asScala
    fromTransaction.map { node =>
      idSet += node.id -> node; node
    } ++ fromParent.filter(n => !idSet.contains(n.id))
  }

  override def hasId(id: Long): Task[Option[Node]] = {
    if (deleted.contains(id)) Task.now(None)
    else
      for {
        r <- super
          .hasId(id)
        r1 <- if (r.nonEmpty) Task.now(r)
        else
          parent.nodes
            .hasId(id)
            .flatMap {
              case Some(node) => _TNode(node.asInstanceOf[parent._Node]).task.map(Some(_))
              case None       => Task.now(None)
            }
      } yield r1
  }

  override def hasId(id: List[Long]): Observable[Node] = {
    Observable
      .fromIterable(id)
      .filter(!deleted.contains(_))
      .flatMap { id =>
        Observable
          .fromTask(for {
            nodeOption <- super.hasId(id)
            nodeOrEdgeOption <- if (nodeOption.nonEmpty) Task.now(nodeOption)
            else
              parent.nodes.hasId(id).flatMap {
                case Some(node) => _TNode(node.asInstanceOf[parent._Node]).task.map(Some(_))
                case None       => Task.now(None)
              }
          } yield nodeOrEdgeOption)
          .map(_.toList)
          .flatMap(Observable.fromIterable(_))
      }
  }
}
