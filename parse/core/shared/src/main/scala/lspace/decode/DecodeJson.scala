package lspace.decode

import java.util.UUID

import lspace.provider.mem.MemGraph
import lspace.structure.{Node, Ontology, Property}
import monix.eval.Task

trait DecodeJson[A] extends Decode[A] {
  def decode: String => Task[A]
}

object DecodeJson {
  case class InvalidJson(message: String)       extends DecodeException(message)
  case class NotAcceptableJson(message: String) extends NotAcceptable(message)

  /**
    *
    * @param label a label which is added to the resulting node
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonToLabeledNode(label: Ontology, allowedProperties: List[Property] = List())(
      implicit decoder: lspace.codec.Decoder): DecodeJson[Node] = new DecodeJson[Node] {
    def decode = (json: String) => {
      val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
      val getProperty = (key: String) => {
        if (allowedProperties.nonEmpty) allowedProperties.find(_.label.get("en").contains(key))
        else Property.properties.cached(key)
      }

      decoder
        .parse(json)
        .flatMap(
          decoder
            .jsonToMap(_)
            .map { obj =>
              Task
                .gatherUnordered(
                  obj.toList.flatMap {
                    case (key, value) =>
                      getProperty(key).map { key =>
                        decoder.toObject(value, key.range)(decoder.getNewActiveContext).map(key -> _)
                      }
                  }
                )
                .map { properties =>
                  val node = resultGraph.nodes.create()
                  node.addLabel(label)
                  properties.foreach { case (p, (ct, v)) => node.addOut(p, ct, v) }
                  node
                }
            }
            .getOrElse(Task.raiseError(new Exception("bad body"))))
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
  def bodyJsonTyped[T](label: Ontology, nodeToT: Node => T, allowedProperties: List[Property] = List())(
      implicit decoder: lspace.codec.Decoder): DecodeJson[T] = new DecodeJson[T] {
    def decode: String => Task[T] = { json: String =>
      jsonToLabeledNode(label).decode(json).map(nodeToT(_))
    }
  }

  /**
    *
    * @param allowedProperties a whitelist for properties which are accepted
    * @param decoder
    * @return
    */
  def jsonToNode(allowedProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder): DecodeJson[Node] =
    new DecodeJson[Node] {
      def decode = (json: String) => {
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)

        val getProperty = (key: String) => {
          if (allowedProperties.nonEmpty) allowedProperties.find(_.label.get("en").contains(key))
          else Property.properties.cached(key)
        }

        decoder
          .parse(json)
          .flatMap(
            decoder
              .jsonToMap(_)
              .map { obj =>
                Task
                  .gatherUnordered(
                    obj.toList.flatMap {
                      case (key, value) =>
                        getProperty(key).map { key =>
                          decoder.toObject(value, key.range)(decoder.getNewActiveContext).map(key -> _)
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
              .getOrElse(Task.raiseError(new Exception("bad body"))))
      }
    }
}
