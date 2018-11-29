package lspace.librarian.provider.mem

import java.util.concurrent.atomic.AtomicLong

import lspace.librarian.structure.Property
import lspace.librarian.structure.util.IdProvider

object MemGraphDefault extends MemDataGraph {
  val iri: String = "memgraphdefault"
  println("create MemGraphDefault")
  private val _iri      = iri
  private lazy val self = this

  lazy val idProvider: IdProvider = new IdProvider {
    private val id = new AtomicLong()
    def next: Long = id.incrementAndGet
  }

  val ns: MemNSGraph = new MemNSGraph {
    def iri: String          = "memgraphdefault" + ".ns"
    private val _iri         = iri
    lazy val graph: MemGraph = self
    private val _thisgraph   = thisgraph
    lazy val index: MemIndexGraph = new MemIndexGraph {
      def iri: String = _iri + ".ns" + ".index"

      lazy val graph: MemGraph      = _thisgraph
      lazy val index: MemIndexGraph = this
    }

    override protected def propertyFromCache(iri: String): Option[Property] =
      Property.allProperties.byIri
        .get(iri)
        .orElse(properties.byIri.get(iri))

    override def getProperty(iri: String): Option[Property] = {
      propertyFromCache(iri)
        .orElse {
          nodeStore.byIri(iri).find(_.hasLabel(Property.ontology).isDefined).map(propertyFromNode)
        }
    }
  }

  val index: MemIndexGraph = new MemIndexGraph {
    def iri: String = _iri + ".index"

    lazy val graph: MemGraph = self
    private val _thisgraph   = thisgraph
    lazy val index: MemIndexGraph = new MemIndexGraph {
      def iri: String = _iri + ".index" + ".index"

      lazy val graph: MemGraph      = _thisgraph
      lazy val index: MemIndexGraph = this
    }
  }

  init()

  println("created MemGraphDefault")
}
