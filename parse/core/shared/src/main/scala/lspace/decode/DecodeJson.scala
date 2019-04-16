package lspace.decode

import java.util.UUID

import lspace.codec.ActiveContext
import lspace.provider.mem.MemGraph
import lspace.structure.{ClassType, Edge, Node, Ontology, Property}
import monix.eval.Task

trait DecodeJson[A] extends Decode[A] {
  def decode: String => Task[A]
}

object DecodeJson {
  case class InvalidJson(message: String)       extends DecodeException(message)
  case class NotAcceptableJson(message: String) extends NotAcceptable(message)

  def jsonToNode(vocabs: List[ActiveContext],
                 allowedProperties: List[Property] = List(),
                 forbiddenProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder) = {
    val getProperty = (key: String) => {
      val expandedKey = vocabs.foldLeft(key) { case (key, context) => context.expandIri(key).iri }
      decoder
        .toProperty(expandedKey)(ActiveContext())
        .onErrorHandle { f =>
          Property.properties.getOrCreate(expandedKey)
        }
        .map { key =>
          key -> vocabs.foldLeft[Option[ClassType[Any]]](None) {
            case (r, context) => r.orElse(context.expectedType(key))
          }
        }
    }

    val validProperty = (property: Property) =>
      if (allowedProperties.nonEmpty) {
        allowedProperties.contains(property) && !forbiddenProperties.contains(property)
      } else if (forbiddenProperties.nonEmpty) {
        !forbiddenProperties.contains(property)
      } else {
        true
    }

    new DecodeJson[Node] {
      def decode =
        (json: String) => {

          decoder
            .parse(json)
            .flatMap(
              decoder
                .jsonToMap(_)
                .map { obj =>
                  Task
                    .gatherUnordered(
                      obj.toList.map {
                        case (key, value) =>
                          getProperty(key).flatMap {
                            case (key, expectedType) =>
                              if (validProperty(key))
                                decoder.toObject(value, expectedType.toList)(ActiveContext()).map(key -> _).map(Some(_))
                              else Task.now(None)
                          }
                      }
                    )
                    .map(_.flatten)
                    .flatMap { properties =>
                      for {
                        node <- decoder.graph.nodes.create()
                        _ <- Task.gatherUnordered(properties.map {
                          case (p, (ct, v)) => node.addOut(p, ct, v)
                        })
                      } yield node
                    }
                }
                .getOrElse(Task.raiseError(new Exception("bad body"))))
        }
    }
  }
//  implicit def decodeJsonEdge(implicit decoder: lspace.codec.Decoder) = new DecodeJson[Edge[Any, Any]] {
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
      forbiddenProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder): DecodeJson[Node] =
    new DecodeJson[Node] {
      def decode = (json: String) => {
        val resultGraph = MemGraph.apply(UUID.randomUUID().toString)
        val getProperty = (key: String) => {
          if (allowedProperties.nonEmpty)
            allowedProperties
              .find(_.label("en").contains(key))
          else if (forbiddenProperties.nonEmpty)
            if (forbiddenProperties.find(_.label("en").contains(key)).isEmpty) Property.properties.get(key) else None
          else Property.properties.get(key)
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
                          decoder.toObject(value, key.range())(ActiveContext()).map(key -> _)
                        }
                    }
                  )
                  .flatMap { properties =>
                    for {
                      node <- resultGraph.nodes.create()
                      _    <- node.addLabel(label)
                      _ <- Task.gatherUnordered(properties.map {
                        case (p, (ct, v)) => node.addOut(p, ct, v)
                      })
                    } yield node
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
  def jsonToNodeToT[T](
      label: Ontology,
      nodeToT: Node => T,
      allowedProperties: List[Property] = List(),
      forbiddenProperties: List[Property] = List())(implicit decoder: lspace.codec.Decoder): DecodeJson[T] =
    new DecodeJson[T] {
      def decode: String => Task[T] = { json: String =>
        jsonToLabeledNode(label, allowedProperties, forbiddenProperties)
          .decode(json)
          .map(nodeToT(_))
      }
    }
}
