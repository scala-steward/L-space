package lspace.structure

import java.util.concurrent.ConcurrentHashMap

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

    def get(node: Node): Task[ClassType[_]] = node.labels match {
      case l if l.contains(DataType.ontology) =>
        datatypes.cached(iri).map(Task.now).getOrElse {
          DataType.build(node).map(_.value())
        }
      case l if l.contains(Property.ontology) =>
        properties.cached(iri).map(Task.now).getOrElse {
          Property.build(node).map(_.value())
        }
      case l if l.contains(Ontology.ontology) =>
        ontologies.cached(iri).map(Task.now).getOrElse {
          Ontology.build(node).map(_.value())
        }
      case _ =>
        Task.raiseError(new Exception(s"could not find class-type ${node.iri}"))
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

    def get(iri: String): Task[Option[Ontology]] =
      Ontology.ontologies
        .get(iri)
        .map(_.map(_.value()).map(Some(_)))
        .getOrElse(
          nodeStore
            .hasIri(iri)
            .find(_.hasLabel(Ontology.ontology).isDefined)
            .find(_.hasLabel(DataType.ontology).isEmpty)
            .map { node =>
              Ontology.ontologies
                .getOrConstruct(node.iri)(
                  Ontology.build(node)
                )
                .map(_.value())
                .map(Some(_))
            }
            .getOrElse(Task.now(None)))

    def get(id: Long): Task[Option[Ontology]] =
      cached(id)
        .map(o => Task.now(Some(o)))
        .getOrElse(
          nodeStore
            .hasId(id)
            .filter(_.hasLabel(Ontology.ontology).isDefined)
            .find(_.hasLabel(DataType.ontology).isEmpty)
            .map { node =>
              Ontology.ontologies
                .getOrConstruct(node.iri)(
                  Ontology.build(node)
                )
                .map(_.value())
                .map(Some(_))
            }
            .getOrElse(Task.now(None)))

    def all: List[Ontology] = byId.values.toList

    def cached(id: Long): Option[Ontology] =
      Ontology.ontologies
        .cached(id)
        .orElse(byId.get(id))

    def cached(iri: String): Option[Ontology] =
      Ontology.ontologies.cached(iri)
//        .orElse(byIri.get(iri))

    def store(ontology: Ontology): Task[Node] = {
      byIri.get(ontology.iri).map(Task.now).getOrElse {
        if (Ontology.ontologies.default.byIri.get(ontology.iri).isDefined) {
          val node = nodes.upsert(ontology.iri, ontology.iris)
          node.addLabel(Ontology.ontology)
          Task.now(node)
        } else {
          nodes
            .hasIri(ontology.iri)
            .find(n => n.hasLabel(Ontology.ontology).isDefined && n.hasLabel(DataType.ontology).isEmpty)
            //      .filter(o => ontology.iris diff o.iris nonEmpty)
            .map(Task.now)
            .getOrElse {
              //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
              //        node.addLabel(Ontology.ontology)
              val node = Ontology.ontologies.default.idByIri
                .get(ontology.iri)
                .map { id =>
                  val node = getOrCreateNode(id)
                  node.addLabel(Ontology.ontology)
                  node
                }
                .getOrElse(nodes.create(Ontology.ontology))

              node.addOut(default.typed.iriUrlString, ontology.iri)
              ontology.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

              for {
                properties      <- Task.gather(ontology.properties.map(ns.properties.store))
                extendedClasses <- Task.gather(ontology.extendedClasses.map(ns.ontologies.store))
              } yield {
                ontology.label.foreach {
                  case (language, label) =>
                    node.addOut(Property.default.`@label`, label).addOut(Property.default.`@language`, language)
                }
                ontology.comment.foreach {
                  case (language, comment) =>
                    node.addOut(Property.default.`@comment`, comment).addOut(Property.default.`@language`, language)
                }
                properties.foreach(edges.create(node, Property.default.`@properties`, _))
//                ontology.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
                extendedClasses.foreach(edges.create(node, Property.default.`@extends`, _))
//                ontology.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))
                ontology.base.foreach { base =>
                  node.addOut(Property.default.`@base`, base)
                }
                byId += node.id   -> ontology
                byIri += node.iri -> node
                ontology.iris.foreach { iri =>
                  byIri += iri -> node
                }
                node
              }
            }
        }
      }
    }
  }
  val ontologies = new Ontologies {}

  trait Properties {
    protected[lspace] val byId: mutable.HashMap[Long, Property] =
      mutable.HashMap[Long, Property]()
    protected[lspace] val byIri: mutable.HashMap[String, Node] =
      mutable.HashMap[String, Node]()

    def get(iri: String): Task[Option[Property]] =
      Property.properties
        .get(iri)
        .map(_.map(_.value()).map(Some(_)))
        .getOrElse(
          nodeStore
            .hasIri(iri)
            .find(_.hasLabel(Property.ontology).isDefined)
            .map { node =>
              Property.properties
                .getOrConstruct(node.iri)(
                  Property.build(node)
                )
                .map(_.value())
                .map(Some(_))
            }
            .getOrElse(Task.now(None)))

    def get(id: Long): Task[Option[Property]] =
      cached(id)
        .map(o => Task.now(Some(o)))
        .getOrElse(
          nodeStore
            .hasId(id)
            .filter(_.hasLabel(Property.ontology).isDefined)
            .map { node =>
              Property.properties
                .getOrConstruct(node.iri)(
                  Property.build(node)
                )
                .map(_.value())
                .map(Some(_))
            }
            .getOrElse(Task.now(None)))

    def all: List[Property] = properties.byId.values.toList

    def cached(id: Long): Option[Property] =
      Property.properties
        .cached(id)
        .orElse(byId.get(id))

    def cached(iri: String): Option[Property] =
      Property.properties.cached(iri)

    //        .orElse(byIri.get(iri))

    def store(property: Property): Task[Node] = {
      byIri.get(property.iri).map(Task.now).getOrElse {
        if (Property.properties.default.byIri.get(property.iri).isDefined) {
          val node = nodes.upsert(property.iri, property.iris)
          node.addLabel(Ontology.ontology)
          Task.now(node)
        } else {
          nodes
            .hasIri(property.iri)
            .find(n => n.hasLabel(Property.ontology).isDefined)
            .map(Task.now)
            //      .filter(o => ontology.iris diff o.iris nonEmpty)
            .getOrElse {
              //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
              //        node.addLabel(Ontology.ontology)
              val node = Property.properties.default.idByIri
                .get(property.iri)
                .map { id =>
                  val node = getOrCreateNode(id)
                  node.addLabel(Property.ontology)
                  node
                }
                .getOrElse(nodes.create(Ontology.ontology))

              node.addOut(default.typed.iriUrlString, property.iri)
              property.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

              for {
                range           <- Task.gather(property.range.map(classtypes.store))
                properties      <- Task.gather(property.properties.map(ns.properties.store))
                extendedClasses <- Task.gather(property.extendedClasses.map(ns.properties.store))
              } yield {

                node.addOut(Property.default.typed.rangeListClassType, range)
                property.containers.foreach { container =>
                  node.addOut(default.`@container`, container)
                }
                property.label.foreach {
                  case (language, label) =>
                    node.addOut(Property.default.`@label`, label).addOut(Property.default.`@language`, language)
                }
                property.comment.foreach {
                  case (language, comment) =>
                    node.addOut(Property.default.`@comment`, comment).addOut(Property.default.`@language`, language)
                }
                properties.foreach(edges.create(node, Property.default.`@properties`, _))
                //                property.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
                extendedClasses.foreach(edges.create(node, Property.default.`@extends`, _))
                //                property.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))
                property.base.foreach { base =>
                  node.addOut(Property.default.`@base`, base)
                }
                byId += node.id   -> property
                byIri += node.iri -> node
                property.iris.foreach { iri =>
                  byIri += iri -> node
                }
                node
              }
            }
        }
      }
    }

    /** Gets all properties which extend key */
    def extending(key: Property): Task[List[Property]] =
      lspace.g.N
        .hasIri(key.iri)
        .repeat(_.in(_.`@extends`), collect = true)()
        .hasLabel(Property.ontology)
        .withGraph(thisgraph)
        .toListF
        .flatMap { nodes =>
          Task.gatherUnordered(nodes.distinct.map { node =>
            cached(node.iri).map(node => Task.now(node)).getOrElse(Property.build(node).map(_.value()))
          })
        }
  }
  val properties = new Properties {}

  trait Datatypes {
    protected[lspace] val byId: mutable.HashMap[Long, DataType[_]] =
      mutable.HashMap[Long, DataType[_]]()
    protected[lspace] val byIri: mutable.HashMap[String, Node] =
      mutable.HashMap[String, Node]()

    def get[T: DefaultsToAny](iri: String): Task[Option[DataType[T]]] =
      DataType.datatypes
        .get(iri)
        .map(_.map(_.value().asInstanceOf[DataType[T]]).map(Some(_)))
        .getOrElse(
          nodeStore
            .hasIri(iri)
            .find(_.hasLabel(DataType.ontology).isDefined)
            .map { node =>
              DataType.datatypes
                .getOrConstruct(node.iri)(
                  DataType.build(node)
                )
                .map(_.value().asInstanceOf[DataType[T]])
                .map(Some(_))
            }
            .getOrElse(Task.now(None)))

    def get[T: DefaultsToAny](id: Long): Task[Option[DataType[T]]] =
      cached(id)
        .map(o => Task.now(Some(o.asInstanceOf[DataType[T]])))
        .getOrElse(
          nodeStore
            .hasId(id)
            .filter(_.hasLabel(DataType.ontology).isDefined)
            .map { node =>
              DataType.datatypes
                .getOrConstruct(node.iri)(
                  DataType.build(node)
                )
                .map(_.value().asInstanceOf[DataType[T]])
                .map(Some(_))
            }
            .getOrElse(Task.now(None)))

    def all: List[DataType[Any]] = datatypes.byId.values.toList

//    protected def fromNode(node: _Node): DataType[_] = {
//      if (graph != MemGraphDefault)
//        byId
//          .get(node.id)
//          .orElse(byIri.get(node.iri))
//          .orElse(MemGraphDefault.ns.datatypes.byId.get(node.id))
//          .orElse(MemGraphDefault.ns.datatypes.byIri.get(node.iri))
//          .get
//      else throw new Exception(s"datatypeFromNode not fully implemented ${node.iri}")
//    } //TODO: retrieve custom collection datatypes

    def cached(id: Long): Option[DataType[_]] =
      DataType.datatypes
        .cached(id)
        .orElse(byId.get(id))

    def cached(iri: String): Option[DataType[_]] =
      DataType.datatypes.cached(iri)
    //        .orElse(byIri.get(iri))

    def store(datatype: DataType[_]): Task[Node] = {
      byIri.get(datatype.iri).map(Task.now).getOrElse {
        if (DataType.datatypes.default.byIri.get(datatype.iri).isDefined) {
          val node = nodes.upsert(datatype.iri, datatype.iris)
          node.addLabel(Ontology.ontology)
          Task.now(node)
        } else {
          nodes
            .hasIri(datatype.iri)
            .find(n => n.hasLabel(DataType.ontology).isDefined)
            //      .filter(o => ontology.iris diff o.iris nonEmpty)
            .map(Task.now)
            .getOrElse {
              //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
              //        node.addLabel(Ontology.ontology)
              val node = DataType.datatypes.default.idByIri
                .get(datatype.iri)
                .map { id =>
                  val node = getOrCreateNode(id)
                  node.addLabel(DataType.ontology)
                  node
                }
                .getOrElse(nodes.create(DataType.ontology))

              node.addOut(default.typed.iriUrlString, datatype.iri)
              datatype.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

              val datatypeTasks = datatype match {
                case dataType: CollectionType[_] =>
                  dataType match {
                    case dataType: ListSetType[Any] =>
                      Seq(store(ListType(dataType.valueRange)).map(dt =>
                        node.addOut(CollectionType.keys.valueRange, List(dt))))
//                      _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                    case dataType: ListType[Any] =>
                      Seq(store(ListType(dataType.valueRange)).map(dt =>
                        node.addOut(CollectionType.keys.valueRange, List(dt))))
//                      _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                    case dataType: MapType[Any, Any] =>
                      Seq(
                        store(ListType(dataType.keyRange)).map(dt => node.addOut(MapType.keys.keyRange, List(dt))),
                        store(ListType(dataType.valueRange)).map(dt =>
                          node.addOut(CollectionType.keys.valueRange, List(dt)))
                      )
//                      _createEdge(node, MapType.keys.keyRange, ListType(dataType.keyRange))
//                      _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                    case dataType: SetType[Any] =>
                      Seq(store(ListType(dataType.valueRange)).map(dt =>
                        node.addOut(CollectionType.keys.valueRange, List(dt))))
//                      _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                    case dataType: VectorType[Any] =>
                      Seq(store(ListType(dataType.valueRange)).map(dt =>
                        node.addOut(CollectionType.keys.valueRange, List(dt))))
//                      _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                    case dataType: Tuple2Type[Any, Any] =>
                      Seq(
                        store(ListType(dataType._1stRange)).map(dt => node.addOut(TupleType.keys._1stRange, List(dt))),
                        store(ListType(dataType._2ndRange)).map(dt => node.addOut(TupleType.keys._2ndRange, List(dt)))
                      )
//                      _createEdge(node, TupleType.keys._1stRange, ListType(dataType._1stRange))
//                      _createEdge(node, TupleType.keys._2ndRange, ListType(dataType._2ndRange))
                    case dataType: Tuple3Type[Any, Any, Any] =>
                      Seq(
                        store(ListType(dataType._1stRange)).map(dt => node.addOut(TupleType.keys._1stRange, List(dt))),
                        store(ListType(dataType._2ndRange)).map(dt => node.addOut(TupleType.keys._2ndRange, List(dt))),
                        store(ListType(dataType._3rdRange)).map(dt => node.addOut(TupleType.keys._3rdRange, List(dt)))
                      )
//                      _createEdge(node, TupleType.keys._1stRange, ListType(dataType._1stRange))
//                      _createEdge(node, TupleType.keys._2ndRange, ListType(dataType._2ndRange))
//                      _createEdge(node, TupleType.keys._3rdRange, ListType(dataType._3rdRange))
                    case dataType: Tuple4Type[Any, Any, Any, Any] =>
                      Seq(
                        store(ListType(dataType._1stRange)).map(dt => node.addOut(TupleType.keys._1stRange, List(dt))),
                        store(ListType(dataType._2ndRange)).map(dt => node.addOut(TupleType.keys._2ndRange, List(dt))),
                        store(ListType(dataType._3rdRange)).map(dt => node.addOut(TupleType.keys._3rdRange, List(dt))),
                        store(ListType(dataType._4rdRange)).map(dt => node.addOut(TupleType.keys._4rdRange, List(dt)))
                      )
//                      _createEdge(node, TupleType.keys._1stRange, ListType(dataType._1stRange))
//                      _createEdge(node, TupleType.keys._2ndRange, ListType(dataType._2ndRange))
//                      _createEdge(node, TupleType.keys._3rdRange, ListType(dataType._3rdRange))
//                      _createEdge(node, TupleType.keys._4rdRange, ListType(dataType._4rdRange))
                    case _ => Seq[Task[Edge[Any, Any]]]()
                  }
                case dataType: DataType[_] => Seq[Task[Edge[Any, Any]]]()
                case _ =>
                  throw new Exception(s"datatype not found?! ${datatype.iri}")
              }
              datatype.label.foreach {
                case (language, label) =>
                  node.addOut(Property.default.`@label`, label).addOut(Property.default.`@language`, language)
              }
              datatype.comment.foreach {
                case (language, comment) =>
                  node.addOut(Property.default.`@comment`, comment).addOut(Property.default.`@language`, language)
              }
              datatype.properties.foreach(_createEdge(node, Property.default.`@properties`, _))

              datatype.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))

              byId += node.id   -> datatype
              byIri += node.iri -> node
              datatype.iris.foreach { iri =>
                byIri += iri -> node
              }

              Task.gather(datatypeTasks).map { edges =>
                node
              }
            }
        }
      }
    }
  }
  val datatypes = new Datatypes {}

  def _createEdge(resource: Resource[_], key: Property, ct: ClassType[_]): Unit =
    edges.create(
      resource,
      key,
      nodes
        .hasIri(ct.iri)
        .headOption
        .getOrElse {
          ct match {
            case ontology: Ontology    => nodes.upsert(ct.iri, Ontology.ontology)
            case property: Property    => nodes.upsert(ct.iri, Property.ontology)
            case datatype: DataType[_] => nodes.upsert(ct.iri, DataType.ontology, Ontology.ontology)
          }
        }
    )
}
