package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.datatype._
import lspace.structure.Property.default
import lspace.structure.util.IdProvider
import lspace.util.types.DefaultsToAny
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture

import scala.collection.{concurrent, mutable}
import scala.collection.JavaConverters._

trait NameSpaceGraph extends DataGraph {
  def ns: this.type = this
  def index: IndexGraph
  def graph: Graph

  lazy val idProvider: IdProvider = graph.idProvider

  override lazy val init: CancelableFuture[Unit] = CancelableFuture.unit //index.init

  trait Classtypes {
    def get(iri: String): Task[Option[ClassType[_]]] =
      datatypes
        .get(iri)
        .flatMap { dt =>
          if (dt.isDefined) Task.now(dt)
          else
            properties.get(iri).flatMap { pt =>
              if (pt.isDefined) Task.now(pt)
              else ontologies.get(iri)
            }
        }

    def get(node: Node): ClassType[_] = node.labels match {
      case l if l.contains(DataType.ontology) =>
        datatypes.cached(iri).getOrElse {
          DataType.datatypes.getAndUpdate(node)
        }
      case l if l.contains(Property.ontology) =>
        properties.cached(iri).getOrElse {
          Property.properties.getAndUpdate(node)
        }
      case l if l.contains(Ontology.ontology) =>
        ontologies.cached(iri).getOrElse {
          Ontology.ontologies.getAndUpdate(node)
        }
      case _ =>
        throw new Exception(s"could not find class-type ${node.iri}")
    }

    def cached(iri: String): Option[ClassType[_]] =
      datatypes
        .cached(iri)
        .orElse(ontologies.cached(iri))
        .orElse(properties.cached(iri))

    def store[CT <: ClassType[_]](classType: CT): Task[Node] = classType match {
      case ontology: Ontology    => ontologies.store(ontology)
      case property: Property    => properties.store(property)
      case datatype: DataType[_] => datatypes.store(datatype)
    }
  }
  val classtypes = new Classtypes {}

  trait Ontologies {
    protected[lspace] val byId: concurrent.Map[Long, Ontology] =
      new ConcurrentHashMap[Long, Ontology]().asScala
    protected[lspace] val byIri: concurrent.Map[String, Node] =
      new ConcurrentHashMap[String, Node]().asScala

    def get(iri: String): Task[Option[Ontology]] = {
      Ontology.ontologies
        .get(iri)
        .map(Coeval.now)
        .map(_.map(Some(_)))
        .map(_.task)
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
                .map(_.task)
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
                .map(_.task)
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
  val ontologies = new Ontologies {}

  trait Properties {
    protected[lspace] val byId: mutable.HashMap[Long, Property] =
      mutable.HashMap[Long, Property]()
    protected[lspace] val byIri: mutable.HashMap[String, Node] =
      mutable.HashMap[String, Node]()

    def get(iri: String): Task[Option[Property]] = {
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
                .now(Property.properties
                  .getAndUpdate(node))
                .map { property =>
                  byId += node.id   -> property
                  byIri += node.iri -> node
                  property.iris.foreach { iri =>
                    byIri += iri -> node
                  }
                  property
                }
                .map(Some(_))
            }.getOrElse(Task.now(None))))
    }

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
                    .now(Property.properties
                      .getAndUpdate(node))
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
            })

    def all: List[Property] = properties.byId.values.toList

    def cached(id: Long): Option[Property] =
      Property.properties
        .cached(id)
        .orElse(byId.get(id))

    def cached(iri: String): Option[Property] =
      Property.properties.get(iri)

    def store(property: Property): Task[Node] = {
      byIri.get(property.iri).map(Task.now).getOrElse {
        if (Property.properties.default.byIri.get(property.iri).isDefined) {
          for {
            node <- nodes.upsert(property.iri, property.iris)
            u <- {
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
                            u    <- node.addLabel(Property.ontology)
                          } yield node
                        }
                        .getOrElse {
                          //                  Property.properties.cache(property)
                          nodes.create(Property.ontology)
                        }
                      iri  <- node.addOut(default.typed.iriUrlString, property.iri)
                      iris <- Task.gather(property.iris.map(iri => node.addOut(default.typed.irisUrlString, iri)))
                    } yield {
                      byId += node.id   -> property
                      byIri += node.iri -> node
                      property.iris.foreach { iri =>
                        byIri += iri -> node
                      }
                      node
                    }
                    range <- Task.gather(property.range().map(classtypes.store))
                    //                properties      <- Task.gather(property.properties.map(ns.properties.store))
                    extendedClasses <- Task.gather(property.extendedClasses().map(ns.properties.store))
                    range           <- node.addOut(Property.default.typed.rangeListClassType, range)
                    extended        <- node.addOut(Label.P.`@extends`, extendedClasses)
                    labels <- Task.gather(property.label().map {
                      case (language, label) =>
                        for {
                          label <- node.addOut(Property.default.`@label`, label)
                          lang  <- label.addOut(Property.default.`@language`, language)
                        } yield label
                    })
                    comments <- Task.gather(property.comment().map {
                      case (language, comment) =>
                        for {
                          comment <- node.addOut(Property.default.`@comment`, comment)
                          lang    <- comment.addOut(Property.default.`@language`, language)
                        } yield comment
                    })
                  } yield {

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
  val properties = new Properties {}

  trait Datatypes {
    protected[lspace] val byId: mutable.HashMap[Long, DataType[_]] =
      mutable.HashMap[Long, DataType[_]]()
    protected[lspace] val byIri: mutable.HashMap[String, Node] =
      mutable.HashMap[String, Node]()

    def get[T: DefaultsToAny](iri: String): Task[Option[DataType[T]]] = {
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
                .now(DataType.datatypes
                  .getAndUpdate(node))
                .map(_.asInstanceOf[DataType[T]])
                .map(Some(_))
            }.getOrElse(Task.now(None))))
    }

    def get[T: DefaultsToAny](id: Long): Task[Option[DataType[T]]] =
      cached(id)
        .map(o => Task.now(Some(o.asInstanceOf[DataType[T]])))
        .getOrElse(
          nodeStore
            .hasId(id)
            .flatMap(_.filter(_.hasLabel(DataType.ontology).isDefined)
              .map { node =>
                Coeval
                  .now(DataType.datatypes
                    .getAndUpdate(node))
                  .map(_.asInstanceOf[DataType[T]])
                  .map(Some(_))
              }
              .getOrElse(Coeval.now(None))
              .task))

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
        if (DataType.datatypes.default.byIri.get(datatype.iri).isDefined) {
          for {
            node <- nodes.upsert(datatype.iri, datatype.iris)
            u <- {
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
                            u    <- node.addLabel(DataType.ontology)
                          } yield node
                        }
                        .getOrElse {
                          //                  DataType.datatypes.cache(datatype)
                          nodes.create(DataType.ontology)
                        }
                      iri  <- node.addOut(default.typed.iriUrlString, datatype.iri)
                      iris <- Task.gather(datatype.iris.map(iri => node.addOut(default.typed.irisUrlString, iri)))
                    } yield {
                      byId += node.id   -> datatype
                      byIri += node.iri -> node
                      datatype.iris.foreach { iri =>
                        byIri += iri -> node
                      }
                      node
                    }
                    datatypes <- Task.gather {
                      datatype match {
                        case dataType: CollectionType[_] =>
                          dataType match {
                            case dataType: ListSetType[Any] =>
                              Seq(store(ListType(dataType.valueRange)).map(dt =>
                                node.addOut(CollectionType.keys.valueRange, List(dt))))
                            case dataType: ListType[Any] =>
                              Seq(store(ListType(dataType.valueRange)).map(dt =>
                                node.addOut(CollectionType.keys.valueRange, List(dt))))
                            case dataType: MapType[Any, Any] =>
                              Seq(
                                store(ListType(dataType.keyRange)).map(dt =>
                                  node.addOut(MapType.keys.keyRange, List(dt))),
                                store(ListType(dataType.valueRange)).map(dt =>
                                  node.addOut(CollectionType.keys.valueRange, List(dt)))
                              )
                            case dataType: SetType[Any] =>
                              Seq(store(ListType(dataType.valueRange)).map(dt =>
                                node.addOut(CollectionType.keys.valueRange, List(dt))))
                            case dataType: VectorType[Any] =>
                              Seq(store(ListType(dataType.valueRange)).map(dt =>
                                node.addOut(CollectionType.keys.valueRange, List(dt))))
                            case dataType: TupleType[Any] =>
                              Seq(Task
                                .gather(dataType.rangeTypes.map(range => Task.gather(range.map(classtypes.store(_)))))
                                .map { nodes =>
                                  node.addOut(TupleType.keys._rangeClassType, nodes)
                                })
                            case _ => Seq[Task[Edge[Any, Any]]]()
                          }
                        case dataType: DataType[_] => Seq[Task[Edge[Any, Any]]]()
                        case _ =>
                          throw new Exception(s"datatype not found?! ${datatype.iri}")
                      }
                    }
                    labels <- Task.gather(datatype.label().map {
                      case (language, label) =>
                        for {
                          label <- node.addOut(Property.default.`@label`, label)
                          lang  <- label.addOut(Property.default.`@language`, language)
                        } yield label
                    })
                    comments <- Task.gather(datatype.comment().map {
                      case (language, comment) =>
                        for {
                          comment <- node.addOut(Property.default.`@comment`, comment)
                          lang    <- comment.addOut(Property.default.`@language`, language)
                        } yield comment
                    })
                    extendedClasses <- Task.gather(datatype.extendedClasses().map(ns.datatypes.store))
                    extended        <- node.addOut(Label.P.`@extends`, datatype.extendedClasses())
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
  val datatypes = new Datatypes {}

  def _createEdge(resource: Resource[_], key: Property, ct: ClassType[_]): Task[Unit] =
    nodes
      .hasIri(ct.iri)
      .headOptionL
      .flatMap(_.map(Task.now).getOrElse {
        ct match {
          case ontology: Ontology    => nodes.upsert(ct.iri, Ontology.ontology)
          case property: Property    => nodes.upsert(ct.iri, Property.ontology)
          case datatype: DataType[_] => nodes.upsert(ct.iri, DataType.ontology, Ontology.ontology)
        }
      })
      .map(edges.create(resource, key, _))
}
