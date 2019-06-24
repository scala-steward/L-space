package lspace.codec.graphql

import lspace._
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.graphql.{GraphQL, Projection, Query}
import lspace.librarian.traversal.{Step, UntypedTraversal}

object Decoder extends Decoder {
  def apply(): Decoder = new Decoder {}

}
//Work-In-Progress
trait Decoder {
  val ignorable      = Set(' ', ',', '\t', '\n')
  val objectStart    = '{'
  val objectEnd      = '}'
  val argumentsStart = '('
  val argumentsEnd   = ')'
  val keyStopper     = Set(':', objectStart, objectEnd, argumentsStart, argumentsEnd) ++ ignorable

  def toGraphQL(graphql: String)(implicit activeContext: ActiveContext): GraphQL = {
//    graphql.dropWhile(ignorable.contains) match {
//      case graphql if graphql.startsWith("{") =>
//    }
    process(graphql) match {
      case (query, "")      => query
      case (query, graphql) => query //TODO: log nonempty graphql-tail
    }
  }

  def process(graphql: String)(implicit activeContext: ActiveContext): (Query, String) = {
    graphql.dropWhile(ignorable.contains) match {
      case graphql if graphql.startsWith("{") =>
        findProjections(graphql.drop(1)) match {
          case (Nil, graphql)         => throw new Exception("empty graphql object?")
          case (projections, graphql) => Query(projections) -> graphql
        }
    }
  }

  def findProjections(graphql: String)(implicit activeContext: ActiveContext): (List[Projection], String) = {
    graphql.dropWhile(ignorable.contains).span(!keyStopper.contains(_)) match {
      case ("", graphql) => List() -> graphql
      case (key, graphql) =>
        val expKey = activeContext.expandIri(key).iri
        val activeProperty = activeContext.definitions
          .get(expKey)
          .getOrElse(ActiveProperty(
            Property.properties.get(expKey).getOrElse(throw new Exception(s"unknown property $expKey")))())
        val projection = Projection(activeProperty.property, activeProperty.`@reverse`)

        graphql.dropWhile(ignorable.contains) match {
          case graphql =>
//            println(s"graphql head ${graphql} :: ${graphql.head}")
            graphql.head match {
              case this.objectStart =>
                process(graphql) match {
                  case (nestedTraversal, graphql) =>
                    findProjections(graphql) match {
                      case (projections, graphql) =>
                        (projection.copy(query = Some(nestedTraversal)) :: projections) -> graphql
                    }
                }
              case this.objectEnd => List(projection) -> graphql.tail.dropWhile(ignorable.contains)
//              case this.argumentsStart => None            -> ""
//              case this.argumentsEnd   => None            -> ""
              case _ =>
                findProjections(graphql) match {
                  case (projections, graphql) =>
                    (projection :: projections) -> graphql.dropWhile(ignorable.contains)
                }
            }
        }
    }
  }

  def findProjection(graphql: String)(implicit activeContext: ActiveContext): (Option[UntypedTraversal], String) = {
    graphql.dropWhile(ignorable.contains) match {
      case graphql if graphql.startsWith("{") =>
        graphql.drop(1).dropWhile(ignorable.contains).span(!keyStopper.contains(_)) match {
          case (key, graphql) =>
            graphql.dropWhile(ignorable.contains) match {
              case graphql =>
                graphql.head match {
                  case this.objectStart    => None -> ""
                  case this.objectEnd      => None -> "" //Some(Project(g.))
                  case this.argumentsStart => None -> ""
                  case this.argumentsEnd   => None -> ""
                }
            }
          //activeContext.expandIri(key).iri
        }
      case _ => None -> graphql
    }
  }

//  def toR(tail: String)(implicit activeContext: ActiveContext): UntypedTraversal = tail.splitAt(1) match {
//    case ("{", tail) => List()
//    case _ =>
//      tail.span(c => !Set('}', '{', ',').contains(c)) match {
//        case (key, tail) if tail.startsWith("{") =>
//          activePropertyToSteps(
//            activeContext.definitions.all
//              .getOrElse(key, throw new Exception("Active Property definition not found"))) ::: toR(tail)
//        case (key, tail) if tail.startsWith("}") =>
//          activePropertyToSteps(
//            activeContext.definitions.all
//              .getOrElse(key, throw new Exception("Active Property definition not found"))) ::: toR(tail)
////        case (key, tail) if tail.startsWith(",") => activePropertyToSteps(
//        //            activeContext.definitions.all
//        //              .get(key)
//        //              .getOrElse(throw new Exception("Active Property definition not found"))) ::: toR(tail)
//      }
//  }

  def activePropertyToSteps(ap: ActiveProperty): UntypedTraversal =
    if (ap.`@reverse`) ap.`@type`.head match {
      case o: Ontology    => g.out(ap.property).hasLabel(o).untyped
      case p: Property    => g.out(ap.property).hasLabel(p).untyped
      case d: DataType[_] => g.out(ap.property).hasLabel(d).untyped
    } else
      ap.`@type`.head match {
        case o: Ontology    => g.in(ap.property).hasLabel(o).untyped
        case p: Property    => g.in(ap.property).hasLabel(p).untyped
        case d: DataType[_] => g.in(ap.property).hasLabel(d).untyped
      }

}
