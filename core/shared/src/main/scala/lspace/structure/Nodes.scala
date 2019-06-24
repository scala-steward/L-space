package lspace.structure

import lspace.Label
import monix.eval.Task
import monix.reactive.Observable

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
      Observable.fromIterable(validIris).flatMap { iri =>
        nodeStore
          .hasIri(iri)
          .asInstanceOf[Observable[Node]]
      } else Observable[Node]()
  }

  def create(ontology: Ontology*): Task[Node] = {
    for {
      id <- idProvider.next
      node = newNode(id)
      u <- Task.sequence(ontology.map(node.addLabel))
    } yield node
  }

  def upsert(iri: String, ontologies: Ontology*): Task[Node] = {
    for {
      node <- upsert(iri, Set[String]())
      u    <- Task.gatherUnordered(ontologies.toList map node.addLabel)
    } yield node
  }

  /**
    *
    * @param iri an iri which should all resolve to the same resource as param uris
    * @param iris a set of iri's which should all resolve to the same resource
    * @return all vertices which identify by the uri's, expected to return (in time) only a single vertex due to eventual consistency
    */
  def upsert(iri: String, iris: Set[String], ontologies: Ontology*): Task[Node] = {
    hasIri(iri :: iris.toList).toListL
      .flatMap {
        case Nil =>
          for {
            node <- create()
            iriEdge <- if (iri.nonEmpty) node.addOut(Label.P.typed.iriUrlString, iri)
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
      .flatMap { node =>
        node.out(lspace.Label.P.`@id`, lspace.Label.P.`@ids`)
        val newIris = ((iris + iri) diff node.iris).toList.filter(_.nonEmpty)
        for {
          iriEdges <- Task.sequence(newIris.map(node.addOut(Label.P.`@ids`, _)))
          _        <- Task.sequence(ontologies.map(node.addLabel))
        } yield node
      }
  }

  def upsert(node: Node): Task[Node] = {
    if (node.graph != thisgraph) { //
      lspace.g.N.out().hasLabel[Double].withGraph(node.graph).toListF
      lspace.g.N.out().hasLabel[Double].head.withGraph(node.graph).headOptionF
      lspace.g.N.out().hasLabel[Double].is(lspace.P.gt(200.0)).withGraph(node.graph).toListF
      lspace.g.N.group(_.out()).withGraph(node.graph).toListF
      lspace.g.N.group(_.out().outMap()).withGraph(node.graph).toListF
      lspace.g.N.group(_.out()).mapValues(_.head).withGraph(node.graph).toListF
      lspace.g.N.group(_.out().path(_.out())).withGraph(node.graph).toListF
      lspace.g.N.path(_.out()).withGraph(node.graph).toListF
      lspace.g.N.path(_.project(_.out()).by(_.out())).withGraph(node.graph).toListF
      lspace.g.N.project(_.out()).withGraph(node.graph).toListF
      lspace.g.V.project(_.hasLabel[Double]).withGraph(node.graph).toListF
      lspace.g.V.project(_.hasLabel[Double].out()).withGraph(node.graph).toListF
      lspace.g.V.project(_.out().hasLabel[Double]).withGraph(node.graph).toListF
      lspace.g.N.project(_.out()).by(_.out()).withGraph(node.graph).toListF
      lspace.g.N.project(_.out()).by(_.out().out()).withGraph(node.graph).toListF
      lspace.g.N.project(_.out()).by(_.out().hasLabel[Double]).withGraph(node.graph).toListF
      lspace.g.N.project(_.out()).by(_.out().is(lspace.P.gt(200.0))).withGraph(node.graph).toListF
      lspace.g.N.project(_.out()).by(_.out().hasLabel[Double].is(lspace.P.gt(200.0))).withGraph(node.graph).toListF
      for {
//        _     <- lspace.g.withGraph(node.graph).toListF
//        _     <- node.g.out().withGraph(node.graph).headF
//        _     <- node.g.out().count().withGraph(node.graph).headF
//        _     <- node.g.out().count().is(lspace.P.eqv(2l)).withGraph(node.graph).headOptionF
//        _     <- node.g.out().head().withGraph(node.graph).headOptionF
//        _     <- node.g.out().max(_.hasLabel[Int]).withGraph(node.graph).headOptionF
//        _     <- node.g.out().dedup().withGraph(node.graph).toSetF
//        _     <- node.g.out().group(_.count).withGraph(node.graph).toListF
//        _     <- node.g.out().group(_.count.is(lspace.P.eqv(2l))).withGraph(node.graph).toListF
//        _     <- node.g.out().group(_.count.constant(1)).withGraph(node.graph).toListF
//        _     <- node.g.out().group(_.head).withGraph(node.graph).toListF
//        _     <- node.g.out().group(_.out()).withGraph(node.graph).toListF
        edges <- node.g.outE().withGraph(node.graph).toListF
        newNode <- if (node.iri.nonEmpty) upsert(node.iri)
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
        newNode <- if (node.iri.nonEmpty) upsert(node.iri, node.iris) else create()
        u       <- Task.gather(node.labels.map(node.addLabel))
        v       <- addMeta(node, newNode)
      } yield node
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
