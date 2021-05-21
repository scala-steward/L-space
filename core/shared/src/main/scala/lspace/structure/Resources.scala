package lspace.structure

import lspace.provider.wrapped.WrappedResource
import lspace.structure.util.UpsertHelper
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.immutable.ListSet

abstract class Resources(val graph: Graph) extends RApi[Resource[Any]] {
  import graph._
  def apply(): Observable[Resource[_]] = nodes() ++ edges() ++ values()
  def count(): Task[Long]              = Task.parSequence(List(nodeStore.count(), edgeStore.count(), valueStore.count())).map(_.sum)

  def hasIri(iris: List[String]): Observable[Resource[Any]] = {
    val validIris = iris.filter(_.nonEmpty)
    if (validIris.nonEmpty) {
      Observable
        .fromIterable(validIris)
        .flatMap(
          iri =>
            nodeStore
              .hasIri(iri)
              .asInstanceOf[Observable[Resource[_]]] ++ edgeStore.hasIri(iri).asInstanceOf[Observable[Resource[_]]])
        .asInstanceOf[Observable[Resource[_]]]
    } else Observable[Resource[_]]()
  }

  def upsert[V](resource: Resource[V])(implicit helper: UpsertHelper = UpsertHelper()): Task[Resource[V]] =
    upsertR(resource)
  //      value match {
  //        case resource: Resource[_] => upsertR(resource).asInstanceOf[Resource[V]]
  //        case value                 => values.create(value).asInstanceOf[Resource[V]]
  //      }
  private def upsertR[V](value: Resource[V])(implicit helper: UpsertHelper): Task[Resource[V]] =
    value match {
      //        case resource: _Resource[V]       => resource
      case resource: WrappedResource[V] => upsertR(resource.self)
      case resource: Resource[V] =>
        resource match {
          case node: _Node =>
            Task.now(node)
          case node: Node =>
            nodes.upsert(node).asInstanceOf[Task[Resource[V]]]
          case edge: _Edge[_, _] => Task.now(edge)
          case edge: Edge[_, _] =>
            edges.upsert(edge).asInstanceOf[Task[Resource[V]]]
          case value: _Value[_] =>
            Task.now(value)
          case value: Value[_] =>
            values.upsert(value).asInstanceOf[Task[Resource[V]]]
          case _ =>
            scribe.error(s"cannot upsert value with class ${value.getClass.toString}")
            Task.raiseError(new Exception("???"))
        }
    }

  def hasId(id: Long): Task[Option[Resource[Any]]] =
    for {
      nodeOption              <- nodes.hasId(id)
      nodeOrEdgeOption        <- if (nodeOption.nonEmpty) Task.now(nodeOption) else edges.hasId(id)
      nodeOrEdgeOrValueOption <- if (nodeOrEdgeOption.nonEmpty) Task.now(nodeOrEdgeOption) else values.hasId(id)
    } yield nodeOrEdgeOrValueOption

  lazy val cached = new {
    def hasId(id: Long): Option[Resource[Any]] =
      nodeStore.cached.hasId(id).orElse(edgeStore.cached.hasId(id)).orElse(valueStore.cached.hasId(id))

    def dereferenceValue(t: Any): Any = t match {
      case v: Vector[_]  => v.map(dereferenceValue)
      case v: ListSet[_] => v.map(dereferenceValue)
      case v: List[_]    => v.map(dereferenceValue)
      case v: Set[_]     => v.map(dereferenceValue)
      case v: Map[_, _] =>
        v.map {
          case (key, value) =>
            dereferenceValue(key) -> dereferenceValue(value)
        }
      case (v1, v2) =>
        (dereferenceValue(v1), dereferenceValue(v2))
      case (v1, v2, v3) =>
        (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3))
      case (v1, v2, v3, v4) =>
        (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4))
      case (v1, v2, v3, v4, v5) =>
        (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4), dereferenceValue(v5))
      //      case v: Ontology    => nodes.upsert(v.iri, Ontology.ontology) //ns.ontologies.store(v))
      //      case v: Property    => nodes.upsert(v.iri, Property.ontology) //(ns.properties.store(v))
      //      case v: DataType[_] => nodes.upsert(v.iri, DataType.ontology) //ns.datatypes.store(v))
      case v: _Node       => v
      case v: Node        => nodeStore.cached.hasId(v.id).getOrElse(newNode(v.id))
      case v: _Edge[_, _] => v
      case v: Edge[_, _] =>
        edgeStore.cached
          .hasId(v.id)
          .getOrElse(
            newEdge[Any, Any](v.id,
                              cache(v.from).asInstanceOf[_Resource[Any]],
                              v.key,
                              cache(v.to).asInstanceOf[_Resource[Any]]))
      case v: _Value[_] => v
      case v: Value[_]  => valueStore.cached.hasId(v.id).getOrElse(newValue(v.id, dereferenceValue(v.value), v.label))
      case _            => t
    }

    def count: Long = nodes.cached.count + edges.cached.count + values.cached.count
  }

  protected[lspace] def cache[T](resource: Resource[T]): _Resource[T] = resource match {
    case node: _Node       => node.asInstanceOf[_Resource[T]]
    case node: Node        => newNode(node.id).asInstanceOf[_Resource[T]]
    case edge: _Edge[_, _] => edge.asInstanceOf[_Resource[T]]
    case edge: Edge[_, _] =>
      newEdge[Any, Any](edge.id,
                        cache(edge.from).asInstanceOf[_Resource[Any]],
                        edge.key,
                        cache(edge.to).asInstanceOf[_Resource[Any]]).asInstanceOf[_Resource[T]]
    case value: _Value[_] => value.asInstanceOf[_Resource[T]]
    case value: Value[_] =>
      newValue(value.id, cached.dereferenceValue(value.value), value.label).asInstanceOf[_Resource[T]]
    case _ => throw new Exception(s"unexpected type ${resource.getClass.getSimpleName}")
  }
}
