package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.datatype.DataType
import lspace.structure.Property.default
import monix.eval.{Coeval, Task}

import scala.collection.concurrent
import scala.collection.JavaConverters._

abstract class Ontologies(val graph: NameSpaceGraph) {
  import graph._

  protected[lspace] val byId: concurrent.Map[Long, Ontology] =
    new ConcurrentHashMap[Long, Ontology]().asScala
  protected[lspace] val byIri: concurrent.Map[String, Node] =
    new ConcurrentHashMap[String, Node]().asScala

  def get(iri: String): Task[Option[Ontology]] = {
    Ontology.ontologies
      .get(iri)
      .map(Coeval.now)
      .map(_.map(Some(_)))
      .map(_.to[Task])
      .getOrElse {
        nodeStore
          .hasIri(iri)
          .find(_.hasLabel(Ontology.ontology).isDefined)
          .findL(_.hasLabel(DataType.ontology).isEmpty)
          .flatMap { node =>
            node
              .map { node =>
                Coeval
                  .now(Ontology.ontologies
                    .getAndUpdate(node))
                  .map { ontology =>
                    byId += node.id   -> ontology
                    byIri += node.iri -> node
                    ontology.iris.foreach { iri =>
                      byIri += iri -> node
                    }
                    ontology
                  }
                  .map(Some(_))
              }
              .map(_.to[Task])
              .getOrElse {
                Task.now(None)
              }
          }
      }
  }

  def get(id: Long): Task[Option[Ontology]] =
    cached(id)
      .map(o => Task.now(Some(o)))
      .getOrElse(
        nodeStore
          .hasId(id)
          .flatMap(
            _.filter(_.hasLabel(Ontology.ontology).isDefined)
              .find(_.hasLabel(DataType.ontology).isEmpty)
              .map { node =>
                Coeval
                  .now(Ontology.ontologies
                    .getAndUpdate(node))
                  .map { ontology =>
                    byId += node.id   -> ontology
                    byIri += node.iri -> node
                    ontology.iris.foreach { iri =>
                      byIri += iri -> node
                    }
                    ontology
                  }
                  .map(Some(_))
              }
              .map(_.to[Task])
              .getOrElse(Task.now(None))))

  def all: List[Ontology] = byId.values.toList

  def cached(id: Long): Option[Ontology] =
    Ontology.ontologies
      .cached(id)
      .orElse(byId.get(id))

  def cached(iri: String): Option[Ontology] =
    Ontology.ontologies.get(iri)

  //        .orElse(byIri.get(iri))

  def store(ontology: Ontology): Task[Node] =
    byIri.get(ontology.iri).map(Task.now).getOrElse {
      if (Ontology.ontologies.default.byIri.get(ontology.iri).isDefined) {
        for {
          node <- nodes.upsert(ontology.iri, ontology.iris)
          u <- {
            byId += node.id   -> ontology
            byIri += node.iri -> node
            ontology.iris.foreach { iri =>
              byIri += iri -> node
            }
            node.addLabel(Ontology.ontology)
          }
        } yield node
      } else {
        nodes
          .hasIri(ontology.iri)
          .findL(n => n.hasLabel(Ontology.ontology).isDefined && n.hasLabel(DataType.ontology).isEmpty)
          //      .filter(o => ontology.iris diff o.iris nonEmpty)
          .flatMap(_.map(Task.now).getOrElse {
            //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
            //        node.addLabel(Ontology.ontology)

            for {
              node <- Ontology.ontologies.default.idByIri
                .get(ontology.iri)
                .map { id =>
                  for {
                    node <- getOrCreateNode(id)
                    u <- {
                      node.addLabel(Ontology.ontology)
                    }
                  } yield node
                }
                .getOrElse {
                  //                  Ontology.ontologies.cache(ontology)
                  nodes.create(Ontology.ontology)
                }
              u    <- node.addOut(default.typed.iriUrlString, ontology.iri)
              iris <- Task.gather(ontology.iris.map(iri => node.addOut(default.typed.irisUrlString, iri)))

              //                properties      <- Task.gather(ontology.properties.map(ns.properties.store))
              extendedClasses <- Task.gather(ontology.extendedClasses().map(ns.ontologies.store))
              extendsE        <- node.addOut(Label.P.`@extends`, extendedClasses)
              labels <- Task.gather(ontology.label().map {
                case (language, label) =>
                  for {
                    label <- node.addOut(Property.default.`@label`, label)
                    lang  <- label.addOut(Property.default.`@language`, language)
                  } yield label
              })
              comments <- Task.gather(ontology.comment().map {
                case (language, comment) =>
                  for {
                    comment <- node.addOut(Property.default.`@comment`, comment)
                    lang    <- comment.addOut(Property.default.`@language`, language)
                  } yield comment
              })
            } yield {
              byId += node.id   -> ontology
              byIri += node.iri -> node
              ontology.iris.foreach { iri =>
                byIri += iri -> node
              }
              //                properties.foreach(edges.create(node, Property.default.`@properties`, _))
              //                properties.foreach(node.addOut(Label.P.`@properties`, _))
              //                ontology.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
              //                extendedClasses.foreach(edges.create(node, Property.default.`@extends`, _))

              //                ontology.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))

              node
            }
          })
      }
    }
}
