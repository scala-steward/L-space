package lspace.codec.turtle

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap

import lspace.codec.ActiveContext
import lspace.structure._
import lspace.types.string.{Blank, Identifier, Iri}
import lspace.types.geo.{Point, Polygon}
import monix.eval.Task
import monix.reactive.Observable

import scala.annotation.tailrec
import scala.collection.concurrent
import scala.collection.immutable.Map
import scala.util.matching.Regex
import scala.jdk.CollectionConverters._
import scala.util.Try

case class Turtle(context: ActiveContext = ActiveContext(), statements: List[Statement] = List())

case class Statement(subject: Identifier, predicates: Predicates)
sealed trait Object
case class Single(value: String)               extends Object
case class Multi(values: List[Object])         extends Object
case class Predicates(pv: List[(Iri, Object)]) extends Object

object Decoder {

  def apply(graph0: Lspace): Decoder =
    new Decoder {
      val graph: Graph = graph0
      lazy val nsDecoder = new Decoder {
        val graph: Graph   = graph0.ns
        lazy val nsDecoder = this
      }
    }
}
trait Decoder {
  def graph: Graph
  def nsDecoder: Decoder

  protected lazy val blankNodes: concurrent.Map[String, Task[Node]] =
    new ConcurrentHashMap[String, Task[Node]](16, 0.9f, 32).asScala
  protected lazy val blankEdges: concurrent.Map[String, Task[Edge[_, _]]] =
    new ConcurrentHashMap[String, Task[Edge[_, _]]](16, 0.9f, 32).asScala
  protected lazy val blankValues: concurrent.Map[String, Task[Value[_]]] =
    new ConcurrentHashMap[String, Task[Value[_]]](16, 0.9f, 32).asScala

  implicit class WithString(s: String) {
    def stripLtGt: String = s.stripPrefix("<").stripSuffix(">")

    def toResource(implicit activeContext: ActiveContext): Task[Resource[_]] =
      s match {
        case nodeLike if nodeLike.startsWith("<") && nodeLike.endsWith(">") =>
          activeContext.expandIri(nodeLike.stripLtGt) match {
            case Iri(iri)   => graph.nodes.upsert(iri)
            case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create())
          }
        case intLike if intLike.endsWith("^^xsd:integer") =>
          graph.values.upsert(intLike.stripSuffix("^^xsd:integer").stripPrefix("\"").stripSuffix("\"").toInt)
        case doubleLike if doubleLike.endsWith("^^xsd:double") =>
          graph.values.upsert(doubleLike.stripSuffix("^^xsd:double").stripPrefix("\"").stripSuffix("\"").toDouble)
        case decimalLike if decimalLike.endsWith("^^xsd:decimal") =>
          graph.values.upsert(decimalLike.stripSuffix("^^xsd:decimal").stripPrefix("\"").stripSuffix("\"").toDouble)
        case booleanLike if Try(booleanLike.toBoolean).isSuccess => graph.values.upsert(booleanLike.toBoolean)
        case stringLike if stringLike.endsWith("^^xsd:string") =>
          graph.values.upsert(stringLike.stripSuffix("^^xsd:string").stripPrefix("\"").stripSuffix("\""))
        case string => graph.values.upsert(string.stripPrefix("\"").stripSuffix("\""))
      }
  }

  implicit class WithTurtle(turtle: Turtle) {
    implicit val activeContext = turtle.context
    def process: Task[Graph] =
      for {
        _ <- Task.parSequence(turtle.statements.map { statement =>
          for {
            subject <- statement.subject match {
              case Iri(iri)   => graph.nodes.upsert(iri)
              case Blank(iri) => blankNodes.getOrElseUpdate(iri, graph.nodes.create())
            }
            p <- statement.predicates.process
            _ <- Task.parSequence(p.map { case (property, resource) => subject --- property --> resource })
          } yield ()
        })
      } yield graph
  }

  implicit class WithPredicates(predicates: Predicates)(implicit activeContext: ActiveContext) {
    def process: Task[List[(Property, Resource[_])]] =
      Task
        .parSequence(predicates.pv.map {
          case (p, Single(o)) => o.toResource.map(r => List(Property.properties.getOrCreate(p.iri, Set()) -> r))
          case (p, Multi(os)) =>
            val property = Property.properties.getOrCreate(p.iri, Set())
            Task.parSequence(os.map {
              case Single(o) => o.toResource.map(property -> _)
              case predicates: Predicates =>
                for {
                  node <- graph.nodes.create()
                  p    <- predicates.process
                  _ <- Task.parSequence(p.map {
                    case (property, resource) => node --- property --> resource
                  })
                } yield property -> node
              case Multi(os) => Task.raiseError(new Exception("unexpected nested multi"))
            })
          case (p, predicates: Predicates) =>
            for {
              node <- graph.nodes.create()
              p2   <- predicates.process
              _ <- Task.parSequence(p2.map {
                case (property, resource) => node --- property --> resource
              })
            } yield List(Property.properties.getOrCreate(p.iri, Set()) -> node)
        })
        .map(_.flatten)
  }

  def parse(string: String): Task[Turtle] = {
    @tailrec
    def spanPrefix(string: String, prefixes: List[String] = List()): (List[String], String) =
      string.trim() match {
        case prefix if prefix.startsWith("@prefix") || prefix.take(7).toLowerCase().startsWith("prefix") =>
          prefix.split("\n", 1).toList match {
            case List(prefix, tail) => spanPrefix(tail, prefixes :+ prefix)
            case List(tail)         => prefixes -> tail
            case _ => throw new Exception("invalid")
          }
        case _ => throw new Exception("invalid")
      }
    val (prefixes, tail) = spanPrefix(string)

    val (headers, contents) = string.linesIterator.toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .span(
        l =>
          l.startsWith("@prefix") || l
            .startsWith("@base") || l.take(7).toLowerCase.startsWith("prefix") || l
            .take(5)
            .toLowerCase
            .startsWith("base"))

    val activeContext = headers.foldLeft(ActiveContext()) {
      case (activeContext, header) if header.startsWith("@prefix") =>
        header.split(" ").toList match {
          case List(key, prefix, iri, ".") =>
            activeContext.copy(
              `@prefix` = activeContext.`@prefix`() + (prefix
                .stripSuffix(":") -> activeContext.expandIri(iri.stripLtGt).iri))
          case other => throw new Exception(s"unexpected @prefix header ${other.mkString(" ")}")
        }
      case (activeContext, header) if header.startsWith("@base") =>
        header.split(" ").toList match {
          case List(key, iri, ".") =>
            activeContext.copy(`@base` = Some(Some(activeContext.expandIri(iri.stripLtGt).iri)))
          case other => throw new Exception(s"unexpected @prefix header ${other.mkString(" ")}")
        }
      case (activeContext, header) =>
        header.split(" ").toList match {
          case List(key, prefix, iri) if key.toLowerCase == "prefix" =>
            activeContext.copy(
              `@prefix` = activeContext.`@prefix`() + (prefix
                .stripSuffix(":") -> activeContext.expandIri(iri.stripLtGt).iri))
          case List(key, iri) if key.toLowerCase == "base" =>
            activeContext.copy(`@base` = Some(Some(activeContext.expandIri(iri.stripLtGt).iri)))
          case other => throw new Exception(s"unexpected header ${other.mkString(" ")}")
        }
    }

    println("activecontext")
    val regex = new Regex("\"(.*?)\"|([^\\s]+)")

    def wordsToStatement(words: List[String]): Statement =
      words match {
        case Nil        => throw new Exception("empty statement")
        case s :: words =>
//          println(s"got subject $s")
//          println(s"tail is $words")
          wordsToPredicates(words) match {
            case (predicates, words) =>
              Statement(activeContext.expandIri(s.stripLtGt), predicates)
          }
      }

    def wordsToPredicates(words: List[String]): (Predicates, List[String]) =
      words match {
        case p :: o :: "." :: Nil =>
          Predicates((activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri] -> Single(o)) :: Nil) -> Nil
        case p :: o :: "," :: words =>
          val pI = activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri]
          wordsToObjects(words, pI) match {
            case (objects, words) =>
              wordsToPredicates(words) match {
                case (predicates, words) =>
                  predicates.copy((pI -> objects.copy(Single(o) :: objects.values)) :: predicates.pv) -> words
              }
          }
        case p :: o :: ";" :: words =>
          val pI = activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri]
          wordsToPredicates(words) match {
            case (predicates, words) => predicates.copy((pI -> Single(o)) :: predicates.pv) -> words
          }
        case p :: o :: "]" :: words =>
          Predicates((activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri] -> Single(o)) :: Nil) -> words
        case p :: "[" :: words =>
          val pI = activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri]
          subjectBlankNode(words, pI) match {
            case (nodePredicates, words) =>
              words match {
                case "." :: Nil => Predicates((pI -> nodePredicates) :: Nil) -> Nil
                case ";" :: words =>
                  wordsToPredicates(words) match {
                    case (predicates, words) => predicates.copy(pv = (pI -> nodePredicates) :: predicates.pv) -> words
                  }
                case _ => throw new Exception("???")
              }
          }
        case _ => throw new Exception("??")
      }

    def subjectBlankNode(words: List[String], predicate: Identifier): (Predicates, List[String]) =
      words match {
        case p :: "[" :: words =>
          val (predicates, tail) = subjectBlankNode(words, activeContext.expandIri(p.stripLtGt))
          Predicates((activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri], predicates) :: Nil) -> tail
        case p :: o :: "]" :: words =>
          Predicates((activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri], Single(o)) :: Nil) -> words
        case p :: o :: ";" :: words =>
          wordsToPredicates(words) match {
            case (predicates, words) =>
              Predicates((activeContext.expandIri(p.stripLtGt).asInstanceOf[Iri], Single(o)) :: predicates.pv) -> words
          }
        case _ => throw new Exception("??")
      }

    def wordsToObjects(words: List[String], predicate: Identifier): (Multi, List[String]) =
      words match {
        case o :: "," :: words =>
          val (oTail, tail) = wordsToObjects(words, predicate)
          oTail.copy(Single(o) :: oTail.values) -> tail
        case o :: ";" :: words => Multi(Single(o) :: Nil) -> (";" :: words)
        case _                 => throw new Exception("??")
      }

    Task.eval {
      Turtle(
        activeContext, {
          val words = regex.findAllIn(contents.mkString(" ")).toList
          words
            .foldLeft(List[List[String]]() -> List[String]()) {
              case ((lines, segments), line) if line.endsWith(".") => ((line :: segments).reverse :: lines) -> Nil
              case ((lines, segments), line)                       => lines                                 -> (line :: segments)
            }
            ._1
            .foldLeft(List[Statement]()) {
              case (statements, words) =>
//                println(s"wordsToStatement: $words")
                wordsToStatement(words) :: statements
            }
        }
      )
    }
  }
}
