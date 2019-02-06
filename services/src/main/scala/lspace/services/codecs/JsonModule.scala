package lspace.services.codecs

import java.util.UUID

import argonaut.Json
import cats.effect.IO
import io.finch._
import io.finch.argonaut.preserveOrder._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.{Node, Ontology, Property}
import lspace.parse.ActiveContext
import lspace.parse.util.NotAcceptableException
import monix.eval.Task
import scribe._

object JsonModule {
  object Encode {}

  object Decode {}
}
trait JsonModule extends Endpoint.Module[IO] {

  import monix.eval.Task.catsEffect
  import monix.execution.Scheduler.global

  def bodyJsonTyped[T](label: Ontology,
                       nodeToT: Node => T,
                       allowedProperties: List[Property] = List()): Endpoint[IO, T] =
    bodyJsonLabeled(label)
      .map { node =>
        if (allowedProperties.nonEmpty) {
          val fNode = DetachedGraph.nodes.create()
          node.outE(allowedProperties: _*).foreach(e => fNode --- e.key --> e.to)
          fNode
        } else node
      }
      .mapOutputAsync(node =>
        Task(nodeToT(node))
          .map(Ok)
          .onErrorHandle {
            case e =>
              e.logger.debug(e.getMessage)
              NotAcceptable(new Exception(s"not a valid ${label.iri}"))
          }
          .toIO(catsEffect(global)))

  def bodyJsonLabeled(label: Ontology, allowedProperties: List[Property] = List()): Endpoint[IO, Node] =
    jsonBody[Json]
      .handle {
        case t => NotAcceptable(new Exception("body is invalid 'application/json'"))
      }
      .mapOutputAsync { json =>
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
        val getProperty = (key: String) => {
          if (allowedProperties.nonEmpty) allowedProperties.find(_.label.get("en").contains(key))
          else ???
        }
        val jsonld = lspace.parse.JsonLD(resultGraph)

        json.obj
          .map { obj =>
            Task
              .gatherUnordered(
                obj.toList.flatMap {
                  case (key, value) =>
                    getProperty(key).map { key =>
                      jsonld.decode.toObject(value, key.range)(ActiveContext()).map(key -> _)
                    }
                }
              )
              .map { properties =>
                val node = DetachedGraph.nodes.create()
                node.addLabel(label)
                properties.foreach { case (p, (ct, v)) => node.addOut(p, ct, v) }
                node
              }
          }
          .map(_.map(Ok)
            .onErrorHandle {
              case e: NotAcceptableException =>
                e.logger.debug(e.getMessage)
                NotAcceptable(new Exception(s"Body does not look like a ${label.iri}"))
            }
            .toIO(catsEffect(global)))
          .getOrElse(Task.now(NotAcceptable(new Exception("bad body"))).toIO(catsEffect(global)))
      }

  def bodyJson(allowedProperties: List[Property] = List()): Endpoint[IO, Node] =
    jsonBody[Json]
      .handle {
        case t => NotAcceptable(new Exception("body is invalid 'application/json'"))
      }
      .mapOutputAsync { json =>
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)

        val getProperty = (key: String) => {
          if (allowedProperties.nonEmpty) allowedProperties.find(_.label.get("en").contains(key))
          else ???
        }
        val jsonld = lspace.parse.JsonLD(resultGraph)

        json.obj
          .map { obj =>
            Task
              .gatherUnordered(
                obj.toList.flatMap {
                  case (key, value) =>
                    getProperty(key).map { key =>
                      jsonld.decode.toObject(value, key.range)(ActiveContext()).map(key -> _)
                    }
                }
              )
              .map { properties =>
                val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
                val node        = resultGraph.nodes.create()
                properties.foreach { case (p, (ct, v)) => node.addOut(p, ct, v) }
                node
              }
          }
          .map(_.map(Ok)
            .onErrorHandle {
              case e: NotAcceptableException =>
                e.logger.debug(e.getMessage)
                NotAcceptable(new Exception(s"Body does not look like a node"))
            }
            .toIO(catsEffect(global)))
          .getOrElse(Task.now(NotAcceptable(new Exception("bad body"))).toIO(catsEffect(global)))
      }
}
