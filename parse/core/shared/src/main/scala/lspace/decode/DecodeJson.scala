package lspace.decode

import java.util.UUID

import lspace.codec.ActiveContext
import lspace.codec.json.jsonld.Decoder
import lspace.provider.mem.MemGraph
import lspace.structure.{ClassType, Edge, Node, Ontology, Property}
import monix.eval.Task

trait DecodeJson[A, F[_]] extends Decode[A, F] {
  type In = String
}

object DecodeJson {
  type Aux[Out, F[_], In0] = DecodeJson[Out, F] { type In = In0 }

  case class InvalidJson(message: String)       extends DecodeException(message)
  case class NotAcceptableJson(message: String) extends NotAcceptable(message)

  def jsonToNode(allowedProperties: List[Property] = List(), forbiddenProperties: List[Property] = List())(
      implicit decoder: Decoder[_]): DecodeJson[Node, Task] = {
    val validProperty = (property: Property) =>
      if (allowedProperties.nonEmpty) {
        allowedProperties.contains(property) && !forbiddenProperties.contains(property)
      } else if (forbiddenProperties.nonEmpty) {
        !forbiddenProperties.contains(property)
      } else {
        true
    }

    if (allowedProperties.nonEmpty || forbiddenProperties.nonEmpty) {
      new DecodeJson[Node, Task] {
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
      new DecodeJson[Node, Task] {
        def decode(implicit activeContext: ActiveContext) = { (json: String) =>
          decoder
            .stringToNode(json)
        }
      }
  }
//  implicit def decodeJsonEdge(implicit decoder: lspace.codec.json.jsonld.Decoder) = new DecodeJson[Edge[Any, Any]] {
//    def decode =
//      (json: String) =>
//        decoder
//          .stringToEdge(json)
//  }

  /**
    *
    * @param label a label which is added to the resulting node
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonToLabeledNode(
      label: Ontology,
      allowedProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: Decoder[_]): DecodeJson[Node, Task] = {
    val filter = if (allowedProperties.nonEmpty) { (node: Node) =>
      val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
      for {
        fNode <- resultGraph.nodes.create()
        _     <- Task.parSequenceUnordered(node.outE(allowedProperties: _*).map(e => fNode --- e.key --> e.to))
      } yield fNode
    } else if (forbiddenProperties.nonEmpty) { (node: Node) =>
      val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
      for {
        fNode <- resultGraph.nodes.create()
        _ <- Task.parSequenceUnordered(
          node.outE().filterNot(forbiddenProperties.contains).map(e => fNode --- e.key --> e.to))
      } yield fNode
    } else { (node: Node) =>
      Task.now(node)
    }
    new DecodeJson[Node, Task] {
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
  def jsonToNodeToT[T](
      label: Ontology,
      nodeToT: Node => T,
      allowedProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: Decoder[_]): DecodeJson[T, Task] =
    new DecodeJson[T, Task] {
      def decode(implicit activeContext: ActiveContext): String => Task[T] = { (json: String) =>
        jsonToLabeledNode(label, allowedProperties, forbiddenProperties)
          .decode(activeContext)(json)
          .map(nodeToT(_))
      }
    }
}
