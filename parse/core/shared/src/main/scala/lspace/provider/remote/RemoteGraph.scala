package lspace.provider.remote

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import lspace.datatype.DataType
import lspace.librarian.task.Guide
import lspace.librarian.traversal.{Collection, Traversal}
import lspace.provider.transaction.Transaction
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.IdProvider
import lspace.structure.{ClassType, Graph, NameSpaceGraph, Property}
import monix.eval.Task
import shapeless.HList
import com.softwaremill.sttp._
import lspace.codec.{jsonld, ActiveContext, NativeTypeDecoder, NativeTypeEncoder}
import lspace.parse.util.HttpClient
import monix.reactive.Observable

object RemoteGraph {
  def apply[F[_], Json](iri: String, host: String, port: Int, path: String)(
      implicit baseEncoder: NativeTypeEncoder.Aux[Json],
      baseDecoder: NativeTypeDecoder.Aux[Json]): RemoteGraph[Json] =
    new RemoteGraph(iri, host, port, path)(baseEncoder, baseDecoder) {}
}
abstract class RemoteGraph[Json](val iri: String, host: String, port: Int, path: String)(
    implicit baseEncoder: NativeTypeEncoder.Aux[Json],
    baseDecoder: NativeTypeDecoder.Aux[Json])
    extends Graph {

  implicit val httpClient: HttpClient = lspace.parse.util.HttpClientImpl
  implicit val backend                = httpClient.backend
  val cache: Graph                    = Graph.apply(iri)

  val encoder: jsonld.Encoder = jsonld.Encoder(baseEncoder)
  val decoder: jsonld.Decoder = jsonld.Decoder(cache)(baseDecoder)

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

  override protected[lspace] def executeTraversal[F[_]](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
      guide: Guide[F]): F[Any] = executeTraversal(traversal).asInstanceOf[F[Any]]
  protected[lspace] def executeTraversal[F[_]](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]): Observable[Any] = {

    Observable
      .fromTask(for {
        node <- traversal.toNode
        json = encoder(node)(ActiveContext()) //TODO: create nice named default context
//        _    = println(json)
        request = sttp
          .body(json)
          .header(HeaderNames.ContentType, "application/ld+json", true)
          .post(serviceUri)
          .response(asStream[Observable[ByteBuffer]])
//        _ = println(request)
        response <- request.send()
      } yield {
        response.body match {
          case Left(error) => Observable.raiseError(new Exception(error))
          case Right(stream) =>
            stream
              .map(StandardCharsets.UTF_8.decode(_).toString())
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

}
