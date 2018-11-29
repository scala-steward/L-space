package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal.P.EqHelper
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.process.traversal.{EqP, P, PredicateCompanion, PredicateWrapper}
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Eqv extends PredicateCompanion("Eqv") with PredicateWrapper[Eqv[_]] {
  ontologyNode --- Property.default.`@extends` --> EqP.ontology

  def wrap(node: Node): Eqv[_] = node match {
    case node: Eqv[_] => node
    case _ =>
      val (pvalue, helper) = EqHelper map node.out(EqP.keys.value).head
      new Eqv(pvalue, node)(helper)
  }

  def apply[T: EqHelper, T0, TT0 <: ClassType[_]](pvalue: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0]): Eqv[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(EqP.keys.value, pvalue)
    new Eqv(pvalue, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Eqv[T] private (val pvalue: T, override val value: Node)(implicit helper: EqHelper[T])
    extends WrappedNode(value)
    with EqP[T] {
  def assert(avalue: Any): Boolean = helper.eqv(avalue, pvalue)

  override def prettyPrint: String = s"eqv($pvalue)"
  //  {
  //    pvalue match {
  //      case pvalue: Resource[_] => avalue match {
  //        case avalue: Resource[_] =>
  //          println(s"comparing two resources by iri ${pvalue.iri} =?= ${avalue.iri}")
  //          pvalue.iri == avalue.iri
  //        case _ =>
  //          println(s"one one resource has an iri ${pvalue.iri} =?= ${avalue}")
  //          false
  //      }
  //      case _ => pvalue == avalue
  //    }
  //  }
}
