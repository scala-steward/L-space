package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.datatype._
import lspace.structure.Property.default
import lspace.structure.util.IdProvider
import lspace.util.types.DefaultsToAny
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture

import scala.collection.{concurrent, mutable}
import scala.collection.JavaConverters._

trait NameSpaceGraph extends DataGraph {
  def ns: this.type = this
  def index: IndexGraph
  def graph: Graph

  lazy val idProvider: IdProvider = graph.idProvider

  override lazy val init: Task[Unit] = Task.unit //index.init

  val classtypes: Classtypes = new Classtypes(this) {}

  val ontologies: Ontologies = new Ontologies(this) {}

  val properties: Properties = new Properties(this) {}

  val datatypes: Datatypes = new Datatypes(this) {}

  protected[lspace] def _createEdge(resource: Resource[_], key: Property, ct: ClassType[_]): Task[Unit] =
    nodes
      .hasIri(ct.iri)
      .headOptionL
      .flatMap(_.map(Task.now).getOrElse {
        ct match {
          case ontology: Ontology    => nodes.upsert(ct.iri, Ontology.ontology)
          case property: Property    => nodes.upsert(ct.iri, Property.ontology)
          case datatype: DataType[_] => nodes.upsert(ct.iri, DataType.ontology, Ontology.ontology)
        }
      })
      .map(edges.create(resource, key, _))

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
      _ = ontologies.byId.clear()
      _ = properties.byId.clear()
      _ = datatypes.byId.clear()
      _ = ontologies.byIri.clear()
      _ = properties.byIri.clear()
      _ = datatypes.byIri.clear()
      _ <- index.purge
    } yield ()
}
