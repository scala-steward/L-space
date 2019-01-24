package lspace.parse

import argonaut.{Json, JsonObject}
import lspace.librarian.structure.{Edge, Property, Resource}
import lspace.NS.types
import lspace.parse.util.FromJsonException

//object LDGraphBuilder {
//
//  def apply(context: ActiveContext = ActiveContext()) =
//    new LDGraphBuilder(context)
//}
//
///**
//  *
//  * @param context
//  * @param `_:` blank nodes
//  */
//class LDGraphBuilder(val context: ActiveContext = ActiveContext()) {
//
//  def copy(context: ActiveContext = this.context,
//           blanks: Map[String, (Resource[_], List[ToLink])] = blankMap): LDGraphBuilder =
//    LDGraphBuilder(context, blanks)
//
//  def withBlanks(iri: String, resource: Resource[_]): LDGraphBuilder = synchronized {
//    if (!blankMap.contains(iri)) copy(blanks = blankMap + (iri -> (resource -> List())))
//    else this
//  }
//  def withBlanks(ir: List[(String, Resource[_])]): LDGraphBuilder = synchronized {
//    ir.filter { case (iri, resource) => !blankMap.contains(iri) }.foldLeft(this) {
//      case (builder, (iri, resource)) => builder.copy(blanks = builder.blankMap + (iri -> (resource -> List())))
//    }
//  }
//
//}
