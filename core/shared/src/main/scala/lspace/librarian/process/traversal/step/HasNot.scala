package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure.{Node, Property, TypedProperty}
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault

object HasNot extends StepDef("HasNot") with StepWrapper[HasNot] {

  def wrap(node: Node): HasNot = node match {
    case node: HasNot => node
    case _ =>
      HasNot(node
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
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/HasNot/Key")
    keyNode.addLabel(Property.ontology)
    keyNode --- Property.default.`@label` --> "Key" --- Property.default.`@language` --> "en"
    keyNode --- Property.default.`@comment` --> "A key" --- Property.default.`@language` --> "en"
    keyNode --- Property.default.`@range` --> Property.ontology

    lazy val key: Property          = Property(keyNode)
    val keyUrl: TypedProperty[Node] = key + Property.ontology

    private val predicateNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/HasNot/Predicate")
    predicateNode.addLabel(Property.ontology)
    predicateNode --- Property.default.`@label` --> "Predicate" --- Property.default.`@language` --> "en"
    predicateNode --- Property.default.`@comment` --> "A Predicate" --- Property.default.`@language` --> "en"
    predicateNode --- Property.default.`@container` --> types.`@list`
    predicateNode --- Property.default.`@range` --> P.ontology

    lazy val predicate: Property          = Property(predicateNode)
    val predicateUrl: TypedProperty[Node] = predicate + P.ontology
  }

  def apply(key: Property, predicates: List[P[_]] = List()): HasNot = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.key, key)

    predicates.foreach(node.addOut(keys.predicate, _))

    HasNot(key, predicates, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.key
  ontologyNode --- Property.default.`@properties` --> keys.predicate
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class HasNot private (key: Property, predicate: List[P[_]], override val value: Node)
    extends WrappedNode(value)
    with HasStep {
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"hasNot(${key.iri}, P.${predicate.head.prettyPrint})"
    else "hasNot(" + key.iri + ")"
}
