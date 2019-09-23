package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.concurrent
import scala.collection.JavaConverters._

abstract class Nodes(val graph: Graph) extends RApi[Node] {
  import graph._

  def apply(): Observable[Node] = nodeStore.all()
  def count(): Task[Long]       = nodeStore.count()

  def hasId(id: Long): Task[Option[Node]] = nodeStore.hasId(id)
  def cached = new {
    def hasId(id: Long): Option[Node] =
      nodeStore.cached.hasId(id)
    def dereferenceValue(t: Any): Any = t
    def count: Long                   = nodeStore.cached.count
  }
  def hasId(id: List[Long]): Observable[Node] = nodeStore.hasId(id)
  override def hasIri(iris: List[String]): Observable[Node] = {
    //    println(s"get nodes $iris")
    val validIris = iris.distinct.filter(_.nonEmpty)
    if (validIris.nonEmpty)
      nodeStore.hasIri(validIris.toSet)
//      Observable.fromIterable(validIris).flatMap { iri =>
//        nodeStore
//          .hasIri(iri)
//          .asInstanceOf[Observable[Node]]
//      }
    else Observable[Node]()
  }

  def create(ontology: Ontology*): Task[Node] = {
    for {
      id <- idProvider.next
      node = newNode(id)
      u <- Task.sequence(ontology.map(node.addLabel))
    } yield node
  }

  def create(iri: String, ontology: Ontology*): Task[Node] = {
    for {
      id <- idProvider.next
      node = newNode(id)
      u <- Task.sequence(ontology.map(node.addLabel))
      _ <- if (iri.nonEmpty) node.addOut(Label.P.`@id`, iri) else Task.unit
      _ <- if (iri.nonEmpty) node.addOut(Label.P.`@ids`, iri) else Task.unit
    } yield node
  }

  def upsert(iri: String, ontologies: Ontology*): Task[Node] = {
    upsert(iri, Set[String](), ontologies: _*)
  }

  private val upsertingTasks: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]]().asScala

  /**
    *
    * @param iri an iri which should all resolve to the same resource as param uris
    * @param iris a set of iri's which should all resolve to the same resource
    * @return all vertices which identify by the uri's, expected to return (in time) only a single vertex due to eventual consistency
    */
  def upsert(iri: String, iris: Set[String], ontologies: Ontology*): Task[Node] = {
    upsertingTasks
      .getOrElseUpdate(
        iri,
        hasIri(iri :: iris.toList).toListL
          .flatMap {
            case Nil =>
              for {
                node <- create()
                _ <- if (iri.nonEmpty) node.addOut(Label.P.typed.iriUrlString, iri)
                else if (iris.headOption.exists(_.nonEmpty))
                  node.addOut(Label.P.typed.iriUrlString, iris.head)
                else Task.unit
              } yield {
                node
              }
            case List(node) => Task.now(node)
            case nodes =>
              mergeNodes(nodes.toSet)
          }
          .doOnFinish(f => Task(upsertingTasks.remove(iri)))
          .memoize
      )
      .flatMap { node =>
        val newIris = ((iris + iri) diff node.iris).toList.filter(_.nonEmpty) //only add new iris
        for {
          _ <- Task.sequence(newIris.map(node.addOut(Label.P.`@ids`, _)))
          _ <- Task.sequence(ontologies.map(node.addLabel))
        } yield node
      }
  }

  def upsert(node: Node): Task[Node] = {
    if (node.graph != thisgraph) { //
      for {
        edges <- node.g.outE().withGraph(node.graph).toListF
        newNode <- if (node.iri.nonEmpty)
          for {
            newNode <- upsert(node.iri)
            _ <- {
              val newIris = ((node.iris) diff newNode.iris).toList.filter(_.nonEmpty) //only add new iris
              for {
                _ <- Task.sequence(newIris.map(newNode.addOut(Label.P.`@ids`, _)))
                _ <- Task.sequence(node.labels.map(newNode.addLabel))
              } yield ()
            }
          } yield newNode
        else {
          for {
            newNode <- create()
            u       <- Task.gather(node.labels.map(newNode.addLabel))
            v       <- addMeta(node, newNode)
          } yield newNode
        }
      } yield newNode
    } else {
      Task.now(node)
    }
  }

  /**
    * adds a node to the graph including all edges and (meta) edges on edges as long as edges have edges
    * @param node
    * @return
    */
  def post(node: Node): Task[Node] = node match {
    case node: _Node => //match on GNode does also accept _Node instances from other Graphs???? Why?
      Task.now(node)
    case _ =>
      for {
        newNode <- if (node.iri.nonEmpty) upsert(node.iri, node.iris) else create() //FIX: ignores node.iris for empty node.iri
        u       <- Task.gather(node.labels.map(newNode.addLabel))
        v       <- addMeta(node, newNode)
      } yield newNode
  }

  final def delete(node: Node): Task[Unit] = node match {
    case node: _Node => deleteNode(node.asInstanceOf[_Node])
    case _           => Task.unit //LOG???
  }

  final def +(label: Ontology): Task[Node] = create(label)

  /**
    * adds a node by reference (iri(s))
    * @param node
    * @return
    */
  final def +(node: Node): Task[Node] = upsert(node)

  /**
    * deletes a node
    * @param node
    */
  final def -(node: Node): Task[Unit] = delete(node)

  /**
    * adds a node by every detail
    * @param node
    * @return
    */
  final def ++(node: Node): Task[Node] = post(node)
}
