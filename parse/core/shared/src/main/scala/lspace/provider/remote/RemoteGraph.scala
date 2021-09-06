package lspace.provider.remote

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import lspace.client.io.HttpClient
import lspace.datatype.DataType
import lspace.librarian.task.Guide
import lspace.librarian.traversal.{Collection, Traversal}
import lspace.provider.transaction.Transaction
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.IdProvider
import lspace.structure.{ClassType, Graph, NameSpaceGraph, Property}
import monix.eval.Task
import shapeless.HList
import sttp.client3._
import sttp.model._
import lspace.codec.json.{JsonDecoder, JsonEncoder}
import lspace.codec.json.jsonld.{JsonLDDecoder, JsonLDEncoder}
import lspace.codec.ActiveContext
import monix.reactive.Observable
import sttp.capabilities.monix.MonixStreams

object RemoteGraph {
  def apply[F[_], Json](iri: String, host: String, port: Int, path: List[String] = List())(
      implicit baseEncoder: JsonEncoder[Json],
      baseDecoder: JsonDecoder[Json],
      httpClient: HttpClient): RemoteGraph[Json] =
    new RemoteGraph(iri, host, port, path)(baseEncoder, baseDecoder, httpClient) {}
}
//TODO: add session/credential config
abstract class RemoteGraph[Json](val iri: String, host: String, port: Int, path: List[String])(
    implicit baseEncoder: JsonEncoder[Json],
    baseDecoder: JsonDecoder[Json],
    httpClient: HttpClient)
    extends Graph {

  val cache: Graph = Graph.apply(iri)

  val encoder: JsonLDEncoder[Json] = JsonLDEncoder(baseEncoder)
  val decoder: JsonLDDecoder[Json] = JsonLDDecoder(cache)(baseDecoder, httpClient)

  val serviceUri = uri"$host:$port/$path"

  override def idProvider: IdProvider = ???

  override def ns: NameSpaceGraph = cache.ns

  override def transaction: Transaction = ???

  override def nodeStore: NodeStore[RemoteGraph.this.type]   = ???
  override def edgeStore: EdgeStore[RemoteGraph.this.type]   = ???
  override def valueStore: ValueStore[RemoteGraph.this.type] = ???

  override def newNode(id: Long): RemoteGraph.this.GNode                                                 = ???
  override def newEdge[S, E](id: Long, from: _Resource[S], key: Property, to: _Resource[E]): GEdge[S, E] = ???
  override def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T]                            = ???

  override def deleteResource[T <: _Resource[_]](resource: T): Task[Unit] = ???

  override def init: Task[Unit] = ???

  override protected[lspace] def traverse[F[_]](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
      guide: Guide[F]): F[Any] = executeTraversal(traversal).asInstanceOf[F[Any]]
  protected[lspace] def executeTraversal[F[_]](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): Observable[Any] =
    Observable
      .fromTask(for {
        node <- traversal.toNode
        json = encoder(node)(ActiveContext()) //TODO: create nice named default context
        request = basicRequest
          .body(json)
          .header(HeaderNames.Accept, "application/ld+json", true)
          .header(HeaderNames.ContentType, "application/ld+json", true)
          //TODO: add cookie/auth headers
          .post(serviceUri)
          .response(asStreamUnsafe(MonixStreams))
        response <- httpClient.backend.flatMap { implicit backend =>
          request.send(backend)
        }
      } yield {
        response.body match {
          case Left(error) => Observable.raiseError(new Exception(error))
          case Right(stream) =>
            stream
              .map(new String(_, StandardCharsets.UTF_8))
              .filter(_.nonEmpty)
              .mapEval(decoder.parse)
              .mapEval(decoder.toNode(_)(ActiveContext())) //TODO: use default nice named context here
              .map(Collection.wrap)
              .map(_.item)
              .flatMap(Observable.fromIterable(_))
        }
      })
      .flatten

//    guide.buildTraversal[Any](traversal)(this)

}
