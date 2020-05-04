package lspace.decode

import java.util.UUID

import lspace.codec.ActiveContext
import lspace.codec.json.jsonld.Decoder
import lspace.librarian.traversal.Traversal
import lspace.provider.mem.MemGraph
import lspace.structure._
import monix.eval.Task
import shapeless.HList

trait DecodeJsonLD[A, F[_]] extends Decode[A, F] {
  type In = String
}

object DecodeJsonLD {
  type Aux[Out, F[_], In0] = DecodeJsonLD[Out, F] { type In = In0 }

  case class InvalidJsonLD(message: String)       extends DecodeException(message)
  case class NotAcceptableJsonLD(message: String) extends NotAcceptable(message)

  /**
    *
    * @param label a label which is added to the resulting node
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonldToLabeledNode(
      label: Ontology,
      allowedProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: Decoder[_]): DecodeJsonLD[Node, Task] = {
    val filter = {
      if (allowedProperties.nonEmpty) { node: Node =>
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
        for {
          fNode <- resultGraph.nodes.create()
          _     <- Task.parSequenceUnordered(node.outE(allowedProperties: _*).map(e => fNode --- e.key --> e.to))
        } yield fNode
      } else if (forbiddenProperties.nonEmpty) { node: Node =>
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
        for {
          fNode <- resultGraph.nodes.create()
          _ <- Task.parSequenceUnordered(
            node.outE().filterNot(forbiddenProperties.contains).map(e => fNode --- e.key --> e.to))
        } yield fNode
      } else { node: Node =>
        Task.now(node)
      }
    }
    new DecodeJsonLD[Node, Task] {
      def decode(implicit activeContext: ActiveContext) =
        (json: String) =>
          decoder
            .stringToLabeledNode(json, label)
            .flatMap(filter)
    }
  }

  /**
    *
    * @param label a label which is added to the resulting node
    * @param nodeToT a function to transform the parsed result to object T
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @tparam T
    * @return
    */
  def bodyJsonldTyped[T](
      label: Ontology,
      nodeToT: Node => T,
      allowedProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: Decoder[_]): DecodeJsonLD[T, Task] = {
    val df = jsonldToLabeledNode(label, allowedProperties, forbiddenProperties)
    new DecodeJsonLD[T, Task] {
      def decode(implicit activeContext: ActiveContext) =
        (json: String) =>
          df.decode(activeContext)(json)
            .map(nodeToT(_))
    }
  }

  /**
    *
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonldToNode(allowedProperties: List[Property] = List(), forbiddenProperties: List[Property] = List())(
      implicit decoder: Decoder[_]): DecodeJsonLD[Node, Task] = {

    val validProperty = (property: Property) =>
      if (allowedProperties.nonEmpty) {
        allowedProperties.contains(property) && !forbiddenProperties.contains(property)
      } else if (forbiddenProperties.nonEmpty) {
        !forbiddenProperties.contains(property)
      } else {
        true
    }

    if (allowedProperties.nonEmpty || forbiddenProperties.nonEmpty) {
      new DecodeJsonLD[Node, Task] {
        def decode(implicit activeContext: ActiveContext) = { (json: String) =>
          decoder
            .stringToNode(json)
            .flatMap { node =>
              val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
              for {
                fNode <- resultGraph.nodes.create()
                _ <- Task.parSequenceUnordered(
                  node.outE().filter(e => validProperty(e.key)).map(e => fNode --- e.key --> e.to))
              } yield fNode
            }
        }
      }
    } else
      new DecodeJsonLD[Node, Task] {
        def decode(implicit activeContext: ActiveContext) = { (json: String) =>
          decoder
            .stringToNode(json)
        }
      }
  }

  def jsonldToEdge(implicit decoder: Decoder[_]) =
    new DecodeJsonLD[Edge[Any, Any], Task] {
      def decode(implicit activeContext: ActiveContext) =
        (json: String) =>
          decoder
            .stringToEdge(json)
    }

  implicit def jsonldToTraversal(
      implicit decoder: Decoder[_]): DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], _ <: HList], Task] =
    new DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], _ <: HList], Task] {
      def decode(implicit activeContext: ActiveContext)
        : String => Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] =
        (string: String) =>
          decoder
            .stringToLabeledNode(string, Traversal.ontology)
            .flatMap { node =>
              Traversal.toTraversal(node)
          }
    }
}
