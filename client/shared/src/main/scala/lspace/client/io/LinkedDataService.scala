package lspace.client.io

import java.nio.ByteBuffer

import argonaut._
import Argonaut._
import com.softwaremill.sttp.{SttpBackend, sttp, _}
import monix.eval.Task
import monix.reactive.Observable
import lspace.librarian.process.traversal._
import lspace.librarian.structure.{ClassType, Node}
import lspace.parse.json.JsonLD
import shapeless.HList

import scala.util.{Failure, Success}

trait LinkedDataService {

  val jsonLD = JsonLD
  implicit def backend: SttpBackend[Task, Observable[ByteBuffer]]
  //  def traverseAsync[Start, End, LastStep, Labels <: HList](traversal: Traversal[Start, End, LastStep, Labels]): Task[List[End]]
  //  def traverse[Steps <: HList, Labels <: HList, Out](traversal: Traversal[_, _, Steps, Labels])(
  //    implicit
  //    ct: ClassType[Out]): Task[Collection[Out]]
  def traverse[Steps <: HList, Out](traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], Steps],
                                    ct: ClassType[Out]): Task[Collection[Out]] = {
    sttp
      .post(uri"${traversal.target.iri}/traverse")
      .body(jsonLD.nodeToJsonWithContext(traversal.self)._1.toString())
      .headers(Map("Content-Type" -> "application/ld+json", "Accept" -> "application/ld+json"))
      .send()
      .map { response =>
        try {
          val json: Json = Parse.parse(response.unsafeBody).right.get
          json.obj
            .map { obj =>
              jsonLD.resource(obj) match {
                case Success(resource) =>
                  Collection.apply(resource.asInstanceOf[Node], ct)
                case Failure(error) =>
                  println(error.getMessage)
                  throw error
              }
            }
            .getOrElse(throw new Exception("result is not a json-object"))
        } catch {
          case e => throw new Exception(e.getMessage)
        }
      }
  }

  def getNode(node: Node): Task[Node] = {
    sttp
      .get(uri"${node.iri}")
      .headers(Map("Content-Type" -> "application/ld+json", "Accept" -> "application/ld+json"))
      .send()
      .map { response =>
        try {
          val json: Json = Parse.parse(response.unsafeBody).right.get
          json.obj
            .map { obj =>
              val cachedNode = jsonLD.resource(obj) match {
                case Success(resource) =>
                  resource match {
                    case node: Node => node //mergeWithCache(node)
                    case _          => throw new Exception("getNode did not return a node")
                  }
                case Failure(error) => throw error
              }
              //          cachedNode.memento := Date.now.toLong
              //          cachedNode.status := CacheStatus.CACHED
              cachedNode
            }
            .getOrElse(throw new Exception("result is not a json-object"))
        } catch {
          case e => throw new Exception(e.getMessage)
        }
      }
  }

  def getNodes(nodes: Seq[Node], uri: String = "data.l-space.eu/api/get"): Task[List[Node]] = {
    sttp
      .post(uri"$uri")
      .body(nodes.toList.map(_.iri.asJson).asJson.toString())
      .headers(Map("Content-Type" -> "application/ld+json", "Accept" -> "application/ld+json"))
      .send()
      .map { response =>
        try {
          val json: Json = Parse.parse(response.unsafeBody).right.get
          json.array
            .map { array =>
              array.map { json =>
                json.obj match {
                  case Some(obj) =>
                    jsonLD.resource(obj) match {
                      case Success(resource) =>
                        resource match {
                          case node: Node => node //mergeWithCache(node)
                          case _          => throw new Exception("getNode did not return a node")
                        }
                      case Failure(error) => throw error
                    }
                  case None => throw new Exception("??")
                }
              }
            }
            .getOrElse(throw new Exception("result is not a json-array"))
        } catch {
          case e => throw new Exception(e.getMessage)
        }
      }
  }
}
