package lspace.decode

import java.util.UUID

import lspace.librarian.traversal.Traversal
import lspace.provider.mem.MemGraph
import lspace.structure._
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
  def jsonldToLabeledNode(
      label: Ontology,
      allowedProperties: List[Property] = List(),
      additionalProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder): DecodeJsonLD[Node] =
    new DecodeJsonLD[Node] {
      def decode =
        (json: String) =>
          decoder
            .stringToLabeledNode(json, label)
            .flatMap { node =>
              if (allowedProperties.nonEmpty) {
                val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
                for {
                  fNode <- resultGraph.nodes.create()
                  _ <- Task.gatherUnordered(
                    node.outE(allowedProperties ++ additionalProperties: _*).map(e => fNode --- e.key --> e.to))
                } yield fNode
              } else if (additionalProperties.nonEmpty) {
                val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
                for {
                  fNode <- resultGraph.nodes.create()
                  _     <- Task.gatherUnordered(node.outE(additionalProperties: _*).map(e => fNode --- e.key --> e.to))
                } yield fNode
              } else if (forbiddenProperties.nonEmpty) {
                val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
                for {
                  fNode <- resultGraph.nodes.create()
                  _ <- Task.gatherUnordered(
                    node.outE().filterNot(forbiddenProperties.contains).map(e => fNode --- e.key --> e.to))
                } yield fNode
              } else Task.now(node)
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
      additionalProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder): DecodeJsonLD[T] =
    new DecodeJsonLD[T] {
      def decode =
        (json: String) =>
          jsonldToLabeledNode(label, allowedProperties, additionalProperties, forbiddenProperties)
            .decode(json)
            .map(nodeToT(_))
    }

  /**
    *
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonldToNode(
      allowedProperties: List[Property] = List(),
      additionalProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder): DecodeJsonLD[Node] =
    new DecodeJsonLD[Node] {
      def decode = { (json: String) =>
        decoder
          .stringToNode(json)
          .flatMap { node =>
            if (allowedProperties.nonEmpty) {
              val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
              for {
                fNode <- resultGraph.nodes.create()
                _ <- Task.gatherUnordered(
                  node.outE(allowedProperties ++ additionalProperties: _*).map(e => fNode --- e.key --> e.to))
              } yield fNode
            } else if (additionalProperties.nonEmpty) {
              val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
              for {
                fNode <- resultGraph.nodes.create()
                _     <- Task.gatherUnordered(node.outE(additionalProperties: _*).map(e => fNode --- e.key --> e.to))
              } yield fNode
            } else if (forbiddenProperties.nonEmpty) {
              val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
              for {
                fNode <- resultGraph.nodes.create()
                _ <- Task.gatherUnordered(
                  node.outE().filterNot(forbiddenProperties.contains).map(e => fNode --- e.key --> e.to))
              } yield fNode
            } else Task.now(node)
          }
      }
    }

  def jsonldToTraversal(
      implicit decoder: lspace.codec.Decoder): DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], HList]] =
    new DecodeJsonLD[Traversal[ClassType[Any], ClassType[Any], HList]] {
      def decode: String => Task[Traversal[ClassType[Any], ClassType[Any], HList]] =
        (string: String) =>
          decoder
            .stringToLabeledNode(string, Traversal.ontology)
            .flatMap { node =>
              Traversal.toTraversal(node)
          }
    }
}
