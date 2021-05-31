package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.structure.Property.default
import monix.eval.Task

import scala.jdk.CollectionConverters._
import scala.collection.concurrent

abstract class Properties(val graph: NameSpaceGraph) {
  import graph._

  protected[lspace] val byId: concurrent.Map[Long, Property] =
    new ConcurrentHashMap[Long, Property]().asScala
  protected[lspace] val byIri: concurrent.Map[String, Node] =
    new ConcurrentHashMap[String, Node]().asScala

  def get(iri: String): Task[Option[Property]] =
    Property.properties
      .get(iri)
      .map(Task.now)
      .map(_.map(Some(_)))
      .getOrElse(
        nodeStore
          .hasIri(iri)
          .findL(_.hasLabel(Property.ontology).isDefined)
          //            .filter(_.out(lspace.Label.P.`@label`).nonEmpty)
          .flatMap(_.map { node =>
            Task
              .now(
                Property.properties
                  .getAndUpdate(node)
              )
              .map { property =>
                byId += node.id   -> property
                byIri += node.iri -> node
                property.iris.foreach { iri =>
                  byIri += iri -> node
                }
                property
              }
              .map(Some(_))
          }.getOrElse(Task.now(None)))
      )

  def get(id: Long): Task[Option[Property]] =
    cached(id)
      .map(o => Task.now(Some(o)))
      .getOrElse(
        nodeStore
          .hasId(id)
          .flatMap {
            _.filter(_.hasLabel(Property.ontology).isDefined)
              .map { node =>
                Task
                  .now(
                    Property.properties
                      .getAndUpdate(node)
                  )
                  .map { property =>
                    byId += node.id   -> property
                    byIri += node.iri -> node
                    property.iris.foreach { iri =>
                      byIri += iri -> node
                    }
                    property
                  }
                  .map(Some(_))
              }
              .getOrElse(Task.now(None))
          }
      )

  def all: List[Property] = properties.byId.values.toList

  def cached(id: Long): Option[Property] =
    Property.properties
      .cached(id)
      .orElse(byId.get(id))

  def cached(iri: String): Option[Property] =
    Property.properties.get(iri)

  def store(property: Property): Task[Node] =
    byIri.get(property.iri).map(Task.now).getOrElse {
      if (Property.properties.default.byIri.get(property.iri).isDefined) {
        for {
          node <- nodes.upsert(property.iri, property.iris)
          _ <- {
            byId += node.id   -> property
            byIri += node.iri -> node
            property.iris.foreach { iri =>
              byIri += iri -> node
            }
            node.addLabel(Property.ontology)
          }
        } yield node
      } else {
        nodes
          .hasIri(property.iri)
          .findL(n => n.hasLabel(Property.ontology).isDefined)
          .flatMap {
            _
              //            .map(Task.now)
              //      .filter(o => ontology.iris diff o.iris nonEmpty)
              .map(Task.now)
              .getOrElse {
                //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
                //        node.addLabel(Ontology.ontology)

                for {
                  node <- for {
                    node <- Property.properties.default.idByIri
                      .get(property.iri)
                      .map { id =>
                        for {
                          node <- getOrCreateNode(id)
                          _    <- node.addLabel(Property.ontology)
                        } yield node
                      }
                      .getOrElse {
                        //                  Property.properties.cache(property)
                        nodes.create(Property.ontology)
                      }
                    _ <- node.addOut(default.typed.iriUrlString, property.iri)
                    _ <- Task.parSequence(property.iris.map(iri => node.addOut(default.typed.irisUrlString, iri)))
                  } yield {
                    byId += node.id   -> property
                    byIri += node.iri -> node
                    property.iris.foreach { iri =>
                      byIri += iri -> node
                    }
                    node
                  }
//                  range <- Task.parSequence(property.range().map(classtypes.store))
                  //                properties      <- Task.parSequence(property.properties.map(ns.properties.store))
                  extendedClasses <- Task.parSequence(property.extendedClasses().collect { case p: Property => p }.map(ns.properties.store))
//                  _               <- node.addOut(Property.default.`@range`, range)
                  _               <- node.addOut(Label.P.`@extends`, extendedClasses)
                  _ <- Task.parSequence(property.label().map { case (language, label) =>
                    for {
                      label <- node.addOut(Property.default.`@label`, label)
                      _     <- label.addOut(Property.default.`@language`, language)
                    } yield label
                  })
                  _ <- Task.parSequence(property.comment().map { case (language, comment) =>
                    for {
                      comment <- node.addOut(Property.default.`@comment`, comment)
                      _       <- comment.addOut(Property.default.`@language`, language)
                    } yield comment
                  })
                } yield

                //                properties.foreach(edges.create(node, Property.default.`@properties`, _))
                //                properties.foreach(node.addOut(Label.P.`@properties`, _))
                //                property.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
                //                extendedClasses.foreach(edges.create(node, Property.default.`@extends`, _))

                //                property.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))
                node
              }
          }
      }
    }

  /** Gets all properties which extend key */
  //    def extending(key: Property): Task[List[Property]] =
  //      lspace.g.N
  //        .hasIri(key.iri)
  //        .repeat(_.in(_.`@extends`), collect = true)()
  //        .hasLabel(Property.ontology)
  //        .withGraph(thisgraph)
  //        .toListF
  //        .flatMap { nodes =>
  //          Coeval
  //            .sequence(nodes.distinct.map { node =>
  //              cached(node.iri).map(node => Coeval.now(node)).getOrElse(Property.properties.getAndUpdate(node))
  //            })
  //            .task
  //        }
}
