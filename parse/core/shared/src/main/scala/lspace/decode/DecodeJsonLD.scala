package lspace.decode

import java.util.UUID

import lspace.librarian.process.traversal.Traversal
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure._
import monix.eval.Task
import shapeless.HList

trait DecodeJsonLD[A] extends Decode[A] {
  def decode: String => Task[A]
}

object DecodeJsonLD {

  case class InvalidJsonLD(message: String)       extends DecodeException(message)
  case class NotAcceptableJsonLD(message: String) extends NotAcceptable(message)

  /**
    *
    * @param label a label which is added to the resulting node
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonldToLabeledNode(label: Ontology, allowedProperties: List[Property] = List())(
      implicit decoder: lspace.codec.Decoder): DecodeJson[Node] = new DecodeJson[Node] {
    def decode =
      (json: String) =>
        decoder
          .stringToLabeledNode(json, label)
          .map { node =>
            if (allowedProperties.nonEmpty) {
              val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
              val fNode       = resultGraph.nodes.create()
              node.outE(allowedProperties: _*).foreach(e => fNode --- e.key --> e.to)
              fNode
            } else node
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
  def bodyJsonldTyped[T](label: Ontology, nodeToT: Node => T, allowedProperties: List[Property] = List())(
      implicit decoder: lspace.codec.Decoder): DecodeJson[T] =
    new DecodeJson[T] {
      def decode = (json: String) => jsonldToLabeledNode(label).decode(json).map(nodeToT(_))
    }

  /**
    *
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonldToNode(allowedProperties: List[Property] = List())(
      implicit decoder: lspace.codec.Decoder): DecodeJson[Node] = new DecodeJson[Node] {
    def decode = { (json: String) =>
      decoder
        .stringToNode(json)
        .map { node =>
          if (allowedProperties.nonEmpty) {
            val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
            val fNode       = resultGraph.nodes.create()
            node.outE(allowedProperties: _*).foreach(e => fNode --- e.key --> e.to)
            fNode
          } else node
        }
    }
  }

  def jsonldToTraversal(implicit decoder: lspace.codec.Decoder,
                        graph: Graph): DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], HList]] =
    new DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], HList]] {
      def decode: String => Task[Traversal[ClassType[Any], ClassType[Any], HList]] =
        (string: String) =>
          decoder
            .stringToLabeledNode(string, Traversal.ontology)
            .map { node =>
              Traversal.toTraversal(node)(graph)
          }
    }
}
