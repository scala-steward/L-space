package lspace.librarian.logic.predicate

//object StartsWithPattern
//    extends PredicateDef("StartsWithPattern", `@extends` = () => List(SeqP.ontology))
//    with PredicateWrapper[StartsWithPattern[_]] {
//
//  def toP(node: Node): StartsWithPattern[_] = {
//
//    val (pvalue, helper) = SeqHelper map node.out(SeqP.keys.value).head
//    StartsWithPattern(pvalue)(helper)
//  }
//
//  object keys extends SeqP.Properties
//  override lazy val properties: List[Property] = SeqP.properties
//  trait Properties extends SeqP.Properties
//
//  implicit def toNode[T](prefix: StartsWithPattern[T]): Node = {
//    val node = DetachedGraph.nodes.create(ontology)
//    node.addOut(SeqP.keys.value, ClassType.valueToOntologyResource(prefix.pvalue), prefix.pvalue)
//    node
//  }
//}
//
//case class StartsWithPattern[T](pvalue: List[P[T]])(implicit helper: SeqHelper[List[P[T]]]) extends SeqP[List[P[T]]] {
//  def assert(avalue: Any): Boolean = helper.startsWith(avalue, pvalue.toList)
//
//  lazy val toNode: Task[Node]            = this
//  override def prettyPrint: String = s"prefix($pvalue)"
//}
