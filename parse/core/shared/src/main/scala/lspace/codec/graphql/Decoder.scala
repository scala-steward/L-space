package lspace.codec.graphql

import lspace._
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.graphql.{GraphQL, Projection, Query}

import scala.annotation.tailrec

object Decoder extends Decoder {
  def apply(): Decoder = new Decoder {}

}
//Work-In-Progress
trait Decoder extends lspace.codec.Decoder {
  val ignorable      = Set(' ', ',', '\t', '\n')
  val objectStart    = '{'
  val objectEnd      = '}'
  val argumentsStart = '('
  val argumentsEnd   = ')'
  val argumentEnds   = ignorable + argumentsEnd
  val keyStopper     = Set(':', objectStart, objectEnd, argumentsStart, argumentsEnd) ++ ignorable

  def toGraphQL(graphql: String)(implicit activeContext: ActiveContext): GraphQL = {
//    graphql.dropWhile(ignorable.contains) match {
//      case graphql if graphql.startsWith("{") =>
//    }
    findQuery(graphql) match {
      case (query, "")      => query
      case (query, graphql) => query //TODO: log nonempty graphql-tail
    }
  }

  def findQuery(graphql: String)(implicit activeContext: ActiveContext): (Query, String) = {
    graphql.dropWhile(ignorable.contains) match {
      case graphql0 if graphql0.startsWith("{") =>
        graphql0.tail.dropWhile(ignorable.contains) match {
          case graphql0 if graphql0.startsWith("_") && graphql0.tail.dropWhile(ignorable.contains).startsWith("(") =>
            processArguments(Query(), graphql0.tail.dropWhile(ignorable.contains).tail) match {
              case (query, graphql) =>
                graphql.dropWhile(ignorable.contains) match {
                  case graphql if graphql.startsWith("{") =>
                    findProjections(graphql.tail) match {
                      case (Nil, graphql) => throw new Exception("empty graphql object?")
                      case (projections, graphql) if graphql.startsWith("}") =>
                        query.copy(query.projections ++ projections) -> graphql.tail.dropWhile(!ignorable.contains(_))
                    }
                }
            }
          case _ =>
            findProjections(graphql0.drop(1)) match {
              case (Nil, graphql)         => throw new Exception(s"empty graphql object? $graphql")
              case (projections, graphql) => Query(projections) -> graphql
            }
        }
    }
  }

  def findProjections(graphql: String)(implicit activeContext: ActiveContext): (List[Projection], String) = {
    graphql.dropWhile(ignorable.contains).span(!keyStopper.contains(_)) match {
      case ("", graphql) => List() -> graphql
      case (key0, graphql0) =>
        val ((key, graphql), projectionName) =
          if (graphql0.startsWith(":"))
            graphql0.tail.dropWhile(ignorable.contains).span(!keyStopper.contains(_)) -> key0
          else key0                                                                   -> graphql0 -> key0
        val expKey = activeContext.expandIri(key).iri
        val activeProperty = activeContext.definitions
          .get(expKey)
          .getOrElse(ActiveProperty(
            Property.properties.get(expKey).getOrElse(throw new Exception(s"unknown property $expKey $graphql0")))())
        val projection = Projection(key, activeProperty.property, projectionName, activeProperty.`@reverse`)

        graphql.dropWhile(ignorable.contains) match {
          case graphql =>
//            println(s"graphql head ${graphql} :: ${graphql.head}")
            graphql.head match {
              case this.objectStart =>
                processObject(projection, graphql.tail)
              case this.objectEnd => List(projection) -> graphql.tail.dropWhile(ignorable.contains)
              case this.argumentsStart =>
                processArguments(projection, graphql.tail)
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

  @tailrec
  final def processArguments(projection: Projection, graphql: String)(
      implicit activeContext: ActiveContext): (List[Projection], String) = {
    graphql.dropWhile(ignorable.contains).span(!keyStopper.contains(_)) match {
      case ("", graphql) => findObject(projection, graphql)
      case (key0, graphql0) if graphql0.startsWith(":") =>
        (graphql0.dropWhile(keyStopper.contains) match {
          case graphql if graphql.startsWith("\"\"\"") =>
            graphql.drop(3).split("\"\"\"", 2).toList match {
              case List(value, graphql) => (key0, value, graphql)
            }
          case graphql if graphql.startsWith("\"") =>
            graphql.tail.split("\"", 2).toList match {
              case List(value, graphql) =>
                (key0, value, graphql)
            }
          case graphql =>
            graphql.span(!argumentEnds.contains(_)) match {
              case (value, graphql) => (key0, value, graphql)
            }
        }) match {
          case (key, value, graphql) =>
            val projection1 = key match {
              case "limit"  => projection.copy(limit = Some(value.toInt))
              case "offset" => projection.copy(offset = Some(value.toInt))
              case _ =>
                val expKey = activeContext.expandIri(key).iri
                val activeProperty = activeContext.definitions
                  .get(expKey)
                  .getOrElse(ActiveProperty(
                    Property.properties.get(expKey).getOrElse(throw new Exception(s"unknown property $expKey")))())
                projection.copy(parameters = projection.parameters + (activeProperty.property -> value))
            }
            graphql.dropWhile(ignorable.contains) match {
              case graphql =>
                //            println(s"graphql head ${graphql} :: ${graphql.head}")
                graphql.head match {
                  //                  case this.objectStart =>
                  //                  case this.objectEnd =>
                  //                  case this.argumentsStart =>
                  case this.argumentsEnd =>
                    findObject(projection1, graphql.tail)
                  case _ =>
                    processArguments(projection1, graphql)
                }
            }
        }
    }
  }

  def findObject(projection: Projection, graphql: String)(
      implicit activeContext: ActiveContext): (List[Projection], String) = {
    graphql.dropWhile(ignorable.contains) match {
      case graphql =>
        graphql.head match {
          case this.objectStart =>
            processObject(projection, graphql.tail)
          case _ =>
            findProjections(graphql) match {
              case (projections, graphql) =>
                (projection :: projections) -> graphql.dropWhile(ignorable.contains)
            }
        }
    }
  }

  def findMoreProjections(projection: Projection, graphql: String)(
      implicit activeContext: ActiveContext): (List[Projection], String) = {
    graphql.dropWhile(ignorable.contains) match {
      case graphql =>
        graphql.head match {
          case this.objectStart => throw new Exception("unexpected start of object")
          case this.objectEnd   => List(projection) -> graphql.tail.dropWhile(ignorable.contains)
          case _ =>
            findProjections(graphql) match {
              case (projections, graphql) =>
                (projection :: projections) -> graphql.dropWhile(ignorable.contains)
            }
        }
    }
  }

  @tailrec
  final def processArguments(query: Query, graphql: String)(implicit activeContext: ActiveContext): (Query, String) = {
    graphql.dropWhile(ignorable.contains).span(!keyStopper.contains(_)) match {
      case ("", graphql) => query -> graphql
      case (key0, graphql0) if graphql0.startsWith(":") =>
        (graphql0.dropWhile(keyStopper.contains) match {
          case graphql if graphql.startsWith("\"\"\"") =>
            graphql.drop(3).split("\"\"\"", 2).toList match {
              case List(value, graphql) => (key0, value, graphql)
            }
          case graphql if graphql.startsWith("\"") =>
            graphql.tail.split("\"", 2).toList match {
              case List(value, graphql) => (key0, value, graphql)
            }
          case graphql =>
            graphql.span(!argumentEnds.contains(_)) match {
              case (value, graphql) => (key0, value, graphql)
            }
        }) match {
          case (key, value, graphql) =>
            val query1 = key match {
              case "limit"  => query.copy(limit = Some(value.toInt))
              case "offset" => query.copy(offset = Some(value.toInt))
              case _ =>
                val expKey = activeContext.expandIri(key).iri
                val activeProperty = activeContext.definitions
                  .get(expKey)
                  .getOrElse(ActiveProperty(
                    Property.properties.get(expKey).getOrElse(throw new Exception(s"unknown property $expKey")))())
                query.copy(parameters = query.parameters + (activeProperty.property -> value))
            }
            graphql.dropWhile(ignorable.contains) match {
              case graphql =>
                //            println(s"graphql head ${graphql} :: ${graphql.head}")
                graphql.head match {
                  //                  case this.objectStart =>
                  //                  case this.objectEnd =>
                  //                  case this.argumentsStart =>
                  case this.argumentsEnd =>
                    query1 -> graphql.tail
                  case _ =>
                    processArguments(query1, graphql)
                }
            }
        }
    }
  }

  def processObject(projection: Projection, graphql: String)(
      implicit activeContext: ActiveContext): (List[Projection], String) = {
    findProjections(graphql) match {
      case (Nil, graphql) => throw new Exception("empty graphql object?")
      case (projections, graphql) =>
        val projection1 = projection.copy(projections = projections)
        findMoreProjections(projection1, graphql)
    }
  }

}
