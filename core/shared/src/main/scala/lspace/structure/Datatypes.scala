package lspace.structure

import lspace.Label
import lspace.datatype._
import lspace.structure.Property.default
import lspace.util.types.DefaultsToAny
import monix.eval.{Coeval, Task}

import java.util.concurrent.ConcurrentHashMap
import scala.collection.concurrent
import scala.jdk.CollectionConverters._

abstract class Datatypes(val graph: NameSpaceGraph) {
  import graph._

  protected[lspace] val byId: concurrent.Map[Long, DataType[_]] =
    new ConcurrentHashMap[Long, DataType[_]]().asScala
  protected[lspace] val byIri: concurrent.Map[String, Node] =
    new ConcurrentHashMap[String, Node]().asScala

  def get[T: DefaultsToAny](iri: String): Task[Option[DataType[T]]] =
    DataType.datatypes
      .get(iri)
      //        .orElse(CollectionType.get(iri))
      .map(Task.now)
      .map(_.map(_.asInstanceOf[DataType[T]]).map(Some(_)))
      .getOrElse(
        nodeStore
          .hasIri(iri)
          .findL(_.hasLabel(DataType.ontology).isDefined)
          .flatMap(_.map { node =>
            Task
              .now(
                DataType.datatypes
                  .getAndUpdate(node)
              )
              .map(_.asInstanceOf[DataType[T]])
              .map(Some(_))
          }.getOrElse(Task.now(None)))
      )

  def get[T: DefaultsToAny](id: Long): Task[Option[DataType[T]]] =
    cached(id)
      .map(o => Task.now(Some(o.asInstanceOf[DataType[T]])))
      .getOrElse(
        nodeStore
          .hasId(id)
          .flatMap(
            _.filter(_.hasLabel(DataType.ontology).isDefined)
              .map { node =>
                Coeval
                  .now(
                    DataType.datatypes
                      .getAndUpdate(node)
                  )
                  .map(_.asInstanceOf[DataType[T]])
                  .map(Some(_))
              }
              .getOrElse(Coeval.now(None))
              .to[Task]
          )
      )

  def all: List[DataType[Any]] = datatypes.byId.values.toList

  def cached(id: Long): Option[DataType[_]] =
    DataType.datatypes
      .cached(id)
      .orElse(byId.get(id))

  def cached(iri: String): Option[DataType[_]] =
    DataType.datatypes.get(iri)

  //        .orElse(byIri.get(iri))

  def store(datatype: DataType[_]): Task[Node] = {
    byIri.get(datatype.iri).map(Task.now).getOrElse {
      if (DataType.datatypes.default.byIri.contains(datatype.iri)) {
        for {
          node <- nodes.upsert(datatype.iri, datatype.iris)
          _ <- {
            byId += node.id   -> datatype
            byIri += node.iri -> node
            datatype.iris.foreach { iri =>
              byIri += iri -> node
            }
            node.addLabel(DataType.ontology)
          }
        } yield node
      } else {
        nodes
          .hasIri(datatype.iri)
          .findL(n => n.hasLabel(DataType.ontology).isDefined)
          .flatMap {
            _
              //      .filter(o => ontology.iris diff o.iris nonEmpty)
              .map(Task.now)
              .getOrElse {
                //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
                //        node.addLabel(Ontology.ontology)
                for {
                  node <- for {
                    node <- DataType.datatypes.default.idByIri
                      .get(datatype.iri)
                      .map { id =>
                        for {
                          node <- getOrCreateNode(id)
                          _    <- node.addLabel(DataType.ontology)
                        } yield node
                      }
                      .getOrElse {
                        //                  DataType.datatypes.cache(datatype)
                        nodes.create(DataType.ontology)
                      }
                    _  <- node.addOut(default.typed.iriUrlString, datatype.iri)
                    _ <- Task.parSequence(datatype.iris.map(iri => node.addOut(default.typed.irisUrlString, iri)))
                  } yield {
                    byId += node.id   -> datatype
                    byIri += node.iri -> node
                    datatype.iris.foreach { iri =>
                      byIri += iri -> node
                    }
                    node
                  }
                  _ <- Task.parSequence {
                    datatype match {
                      case dataType: CollectionType[_] =>
                        dataType match {
                          case dataType: ListSetType[_] =>
                            Seq(
                              store(dataType.valueRange.map(ListType(_)).getOrElse(ListType()))
                                .map(dt => node.addOut(CollectionType.keys.valueRange, List(dt)))
                            )
                          case dataType: ListType[_] =>
                            Seq(
                              store(dataType.valueRange.map(ListType(_)).getOrElse(ListType()))
                                .map(dt => node.addOut(CollectionType.keys.valueRange, List(dt)))
                            )
                          case dataType: MapType[_] =>
                            Seq(
                              store(dataType.keyRange.map(ListType(_)).getOrElse(ListType()))
                                .map(dt => node.addOut(MapType.keys.keyRange, List(dt))),
                              store(dataType.valueRange.map(ListType(_)).getOrElse(ListType()))
                                .map(dt => node.addOut(CollectionType.keys.valueRange, List(dt)))
                            )
                          case dataType: SetType[_] =>
                            Seq(
                              store(dataType.valueRange.map(ListType(_)).getOrElse(ListType()))
                                .map(dt => node.addOut(CollectionType.keys.valueRange, List(dt)))
                            )
                          case dataType: VectorType[_] =>
                            Seq(
                              store(dataType.valueRange.map(ListType(_)).getOrElse(ListType()))
                                .map(dt => node.addOut(CollectionType.keys.valueRange, List(dt)))
                            )
                          case dataType: TupleType[_] =>
                            Seq(
                              Task
                                .parSequence(
                                  dataType.rangeTypes
                                    .map(range => Task.parSequence(range.toList.map(classtypes.store(_))))
                                )
                                .map { nodes =>
                                  node.addOut(TupleType.keys._rangeClassType, nodes.map(_.headOption))
                                }
                            )
                          case _ => Seq[Task[Edge[Any, Any]]]()
                        }
                      case _: DataType[_] => Seq[Task[Edge[Any, Any]]]()
                      case _ =>
                        throw new Exception(s"datatype not found?! ${datatype.iri}")
                    }
                  }
                  _ <- Task.parSequence(datatype.label().map { case (language, label) =>
                    for {
                      label <- node.addOut(Property.default.`@label`, label)
                      _     <- label.addOut(Property.default.`@language`, language)
                    } yield label
                  })
                  _ <- Task.parSequence(datatype.comment().map { case (language, comment) =>
                    for {
                      comment <- node.addOut(Property.default.`@comment`, comment)
                      _       <- comment.addOut(Property.default.`@language`, language)
                    } yield comment
                  })
                  _ <- Task.parSequence(datatype.extendedClasses().map(ns.datatypes.store))
                  _ <- node.addOut(Label.P.`@extends`, datatype.extendedClasses())
                } yield node
                //              datatype.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
                //              datatype.properties.foreach(node.addOut(Label.P.`@properties`, _))

                //              datatype.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))
              }
          }
      }
    }
  }
}
