package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, LUBConstraint}

object Has extends StepDef("Has") with StepWrapper[Has] {

  def wrap(node: Node): Has = node match {
    case node: Has => node
    case _ =>
      Has(node
            .outE(keys.key)
            .take(1)
            .map(i => node.graph.ns.getProperty(i.inV.iri).get)
            .head,
          node
            .out(keys.predicateUrl)
            .map(P.wrap),
          node)
  }

  object keys {
    private val keyNode =
      MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Has/Key")
    keyNode.addLabel(Property.ontology)
    keyNode --- Property.default.label --> "Key" --- Property.default.language --> "en"
    keyNode --- Property.default.comment --> "A key" --- Property.default.language --> "en"
    keyNode --- Property.default.range --> Property.ontology //DataType.default.propertyURLType

    lazy val key: Property          = Property(keyNode)
    val keyUrl: TypedProperty[Node] = key + Property.ontology //DataType.default.propertyURLType

    private val predicateNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Has/Predicate")
    predicateNode.addLabel(Property.ontology)
    predicateNode --- Property.default.label --> "Predicate" --- Property.default.language --> "en"
    predicateNode --- Property.default.comment --> "A Predicate" --- Property.default.language --> "en"
    predicateNode --- Property.default.container --> types.list
    predicateNode --- Property.default.range --> P.ontology

    lazy val predicate: Property          = Property(predicateNode)
    val predicateUrl: TypedProperty[Node] = predicate + P.ontology
  }

  def apply(key: Property, predicates: List[P[_]] = List()): Has = {
    val node = DetachedGraph.createNode(ontology)

    node.addOut(keys.key, key)
    predicates.foreach(predicate => node.addOut(keys.predicateUrl, predicate))
    Has(key, predicates, node)
  }

  ontologyNode --- Property.default.properties --> keys.key
  ontologyNode --- Property.default.properties --> keys.predicate
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Has private (key: Property, predicate: List[P[_]], override val value: Node)
    extends WrappedNode(value)
    with HasStep {
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"has(${key.iri}, P.${predicate.head.prettyPrint})"
    else "has(" + key.iri + ")"
}
