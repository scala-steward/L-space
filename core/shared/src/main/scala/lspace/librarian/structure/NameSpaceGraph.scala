package lspace.librarian.structure

import lspace.librarian.datatype._
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Property.default
import lspace.librarian.structure.util.IdProvider
import lspace.util.types.DefaultsToAny
import monix.execution.CancelableFuture

import scala.collection.mutable

trait NameSpaceGraph extends DataGraph {
  def ns: this.type = this
  def index: IndexGraph
  def graph: Graph

  lazy val idProvider: IdProvider = graph.idProvider

  override lazy val init: CancelableFuture[Unit] = index.init

  trait Classtypes {
    def get(iri: String): Option[ClassType[_]] =
      datatypes
        .get(iri)
        .orElse(MemGraphDefault.ns.datatypes.get(iri))
        .orElse(properties.get(iri).orElse(MemGraphDefault.ns.properties.get(iri)))
        .orElse(ontologies.get(iri).orElse(MemGraphDefault.ns.ontologies.get(iri)))

    def get(node: Node): ClassType[_] = node.labels match {
      case l if l.contains(DataType.ontology) =>
        datatypes
          .get(node.iri)
          .orElse(MemGraphDefault.ns.datatypes.get(node.iri))
          .getOrElse(DataType.apply(node))
      case l if l.contains(Property.ontology) =>
        properties
          .get(node.iri)
          .orElse(MemGraphDefault.ns.properties.get(node.iri))
          .getOrElse(Property.apply(node))
      case l if l.contains(Ontology.ontology) =>
        ontologies
          .get(node.iri)
          .orElse(MemGraphDefault.ns.ontologies.get(node.iri))
          .getOrElse(Ontology.apply(node))
      case _ =>
        throw new Exception(s"could not find class-type ${node.iri}")
    }

    def store[CT <: ClassType[_]](classType: CT): Node = classType match {
      case ontology: Ontology    => ontologies.store(ontology)
      case property: Property    => properties.store(property)
      case datatype: DataType[_] => datatypes.store(datatype)
    }
  }
  val classtypes = new Classtypes {}

  trait Ontologies {
    protected[librarian] val byId: mutable.HashMap[Long, Ontology] =
      mutable.HashMap[Long, Ontology]()
    protected[librarian] val byIri: mutable.HashMap[String, Ontology] =
      mutable.HashMap[String, Ontology]()

    def get(iri: String): Option[Ontology] =
      cached(iri)
        .orElse {
          nodeStore
            .hasIri(iri)
            .find(_.hasLabel(Ontology.ontology).isDefined)
            .find(_.hasLabel(DataType.ontology).isEmpty)
            .map(fromNode)
        }

    def get(id: Long): Option[Ontology] =
      cached(id)
        .orElse(
          nodeStore
            .hasId(id)
            .filter(_.hasLabel(Ontology.ontology).isDefined)
            .find(_.hasLabel(DataType.ontology).isEmpty)
            .map(fromNode))

    def all: List[Ontology] = ontologies.byId.values.toList

    protected def fromNode(node: _Node): Ontology = {
      val ontology = Ontology(node)
      ontologies.byId += node.id -> ontology
      ontologies.byIri += iri    -> ontology
      ontology.iris.foreach(iri => ontologies.byIri += iri -> ontology)
      if (graph != MemGraphDefault) MemGraphDefault.ns.ontologies.store(ontology)
      ontology
    }

    def cached(id: Long): Option[Ontology] =
      Ontology.allOntologies.byId
        .get(id)
        .orElse(ontologies.byId.get(id))

    def cached(iri: String): Option[Ontology] =
      Ontology.allOntologies.byIri
        .get(iri)
        .orElse(ontologies.byIri
          .get(iri))
        .orElse(
          MemGraphDefault.ns.ontologies
            .get(iri)
            .map { ontology =>
              store(ontology)
              ontology
            })

    def store(ontology: Ontology): Node = {
      if (graph != MemGraphDefault) MemGraphDefault.ns.ontologies.store(ontology)
      if (Ontology.allOntologies.byIri.get(ontology.iri).isDefined) {
        val node = nodes.upsert(ontology.iri, ontology.iris)
        node.addLabel(Ontology.ontology)
        node
      } else
        nodes
          .hasIri(ontology.iri)
          .find(n => n.hasLabel(Ontology.ontology).isDefined && n.hasLabel(DataType.ontology).isEmpty)
          //      .filter(o => ontology.iris diff o.iris nonEmpty)
          .getOrElse {
            //        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
            //        node.addLabel(Ontology.ontology)
            val node = Ontology.allOntologies.idByIri
              .get(ontology.iri)
              .map { id =>
                val node = getOrCreateNode(id)
                node.addLabel(Ontology.ontology)
                node
              }
              .getOrElse(nodes.create(Ontology.ontology))

            node.addOut(default.typed.iriUrlString, ontology.iri)
            ontology.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

            ontology.label.foreach {
              case (language, label) =>
                node.addOut(Property.default.`@label`, label).addOut(Property.default.`@language`, language)
            }
            ontology.comment.foreach {
              case (language, comment) =>
                node.addOut(Property.default.`@comment`, comment).addOut(Property.default.`@language`, language)
            }
            ontology.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
            ontology.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))
            ontology.base.foreach { base =>
              node.addOut(Property.default.`@base`, base)
            }
            ontologies.byId += node.id       -> ontology
            ontologies.byIri += ontology.iri -> ontology
            ontology.iris.foreach { iri =>
              ontologies.byIri += iri -> ontology
            }
            node
          }
    }
  }
  val ontologies = new Ontologies {}

  trait Properties {
    protected[librarian] val byId: mutable.HashMap[Long, Property] =
      mutable.HashMap[Long, Property]()
    protected[librarian] val byIri: mutable.HashMap[String, Property] =
      mutable.HashMap[String, Property]()

    def get(id: Long): Option[Property] =
      cached(id)
        .orElse(nodeStore.hasId(id).find(_.hasLabel(Property.ontology).isDefined).map(fromNode))

    def get(iri: String): Option[Property] =
      cached(iri)
        .orElse {
          nodeStore.hasIri(iri).find(_.hasLabel(Property.ontology).isDefined).map(fromNode)
        }

    def all: List[Property] = properties.byId.values.toList

    protected def fromNode(node: _Node): Property = {
      val property = Property(node)
      if (graph != MemGraphDefault && MemGraphDefault.ns.properties.get(node.iri).isEmpty) {
        MemGraphDefault.ns.properties.store(property)
      }
      byIri += iri    -> property
      byId += node.id -> property
      property.iris.foreach(iri => byIri += iri -> property)
      property
    }

    def cached(id: Long): Option[Property] =
      properties.byId
        .get(id)
        .orElse(
          Property.allProperties.byId
            .get(id)
            .map { property =>
              store(property)
              property
            })

    def cached(iri: String): Option[Property] =
      Property.allProperties.byIri
        .get(iri)
        .orElse(properties.byIri
          .get(iri))
        .orElse(
          MemGraphDefault.ns.properties
            .get(iri)
            .map { property =>
              store(property)
              property
            })

    def store(property: Property): Node = {
      if (graph != MemGraphDefault) MemGraphDefault.ns.properties.store(property)
      if (Property.allProperties.byIri
            .get(property.iri)
            .isDefined) { //check if it a default property which is always available by the L-space framework
        val node = nodes.upsert(property.iri, property.iris)
        node.addLabel(Property.ontology)
        node
      } else
        nodes.hasIri(property.iri).find(_.hasLabel(Property.ontology).isDefined).getOrElse {
          //      val node = ns.nodes.upsert(property.iri, property.iris)
          //      node.addLabel(Property.ontology)
          val node = Property.allProperties.idByIri
            .get(property.iri)
            .map { id =>
              val node = getOrCreateNode(id)
              node.addLabel(Property.ontology)
              node
            }
            .getOrElse(nodes.create(Property.ontology))

          node.addOut(default.typed.iriUrlString, property.iri)
          property.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

          node.addOut(Property.default.typed.rangeListClassType, property.range.map(classtypes.store))
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
          property.properties.foreach(_createEdge(node, Property.default.`@properties`, _))
          property.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))
          property.base.foreach { base =>
            node.addOut(Property.default.`@base`, base)
          }
          properties.byId += node.id       -> property
          properties.byIri += property.iri -> property
          property.iris.foreach { iri =>
            properties.byIri += iri -> property
          }
          node
        }
    }

    /** Gets all properties which extend key */
    def extending(key: Property): List[Property] = {
      g.N
        .hasIri(key.iri)
        .repeat(_.in(_.`@extends`), collect = true)
        .hasLabel(Property.ontology)
        .toList
        .distinct
        .map(_.iri)
        .flatMap(properties.get(_))
    }
  }
  val properties = new Properties {}

  trait Datatypes {
    protected[librarian] val byId: mutable.HashMap[Long, DataType[_]] =
      mutable.HashMap[Long, DataType[_]]()
    protected[librarian] val byIri: mutable.HashMap[String, DataType[_]] =
      mutable.HashMap[String, DataType[_]]()

    def get[T: DefaultsToAny](iri: String): Option[DataType[T]] = {
      cached(iri)
        .orElse(nodeStore.hasIri(iri).find(_.hasLabel(DataType.ontology).isDefined).map(fromNode))
        .asInstanceOf[Option[DataType[T]]]
    }

    def get[T: DefaultsToAny](id: Long): Option[DataType[T]] = {
      datatypes
        .cached(id)
        .orElse(nodeStore.hasId(id).find(_.hasLabel(DataType.ontology).isDefined).map(fromNode))
        .asInstanceOf[Option[DataType[T]]]
    }

    def all: List[DataType[Any]] = datatypes.byId.values.toList

    protected def fromNode(node: _Node): DataType[_] = {
      if (graph != MemGraphDefault)
        byId
          .get(node.id)
          .orElse(byIri.get(node.iri))
          .orElse(MemGraphDefault.ns.datatypes.byId.get(node.id))
          .orElse(MemGraphDefault.ns.datatypes.byIri.get(node.iri))
          .get
      else throw new Exception(s"datatypeFromNode not fully implemented ${node.iri}")
    } //TODO: retrieve custom collection datatypes

    def cached(id: Long): Option[DataType[_]] =
      datatypes.byId
        .get(id)
        .orElse(
          DataType.allDataTypes.byId
            .get(id)
            .map { datatype =>
              store(datatype)
              datatype
            })

    def cached(iri: String): Option[DataType[_]] =
      DataType.allDataTypes.byIri
        .get(iri)
        .orElse(datatypes.byIri
          .get(iri))
        .orElse(
          MemGraphDefault.ns.datatypes
            .get(iri)
            .map { datatype =>
              store(datatype)
              datatype
            })

    def store(dataType: DataType[_]): Node = {
      if (DataType.allDataTypes.byIri.get(dataType.iri).isDefined) {
        val node = nodes.upsert(dataType.iri, dataType.iris)
        node.addLabel(Ontology.ontology) //TODO: redundant? removed by addLabel(DataType.ontology) because DataType extends Ontology
        node.addLabel(DataType.ontology)
        node
      } else {
        nodes
          .hasIri(dataType.iri)
          .find(_.hasLabel(DataType.ontology).isDefined)
          .getOrElse {
            //      val node = ns.nodes.upsert(dataType.iri, dataType.iris)
            val node = DataType.allDataTypes.idByIri
              .get(dataType.iri)
              .map { id =>
                val node = getOrCreateNode(id)
                node.addLabel(Ontology.ontology)
                node.addLabel(DataType.ontology)
                node
              }
              .getOrElse(nodes.create(Ontology.ontology, DataType.ontology))
            node.addOut(default.typed.iriUrlString, dataType.iri)
            dataType.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

            datatypes.byIri += dataType.iri -> dataType
            datatypes.byId += node.id       -> dataType
            dataType.iris.foreach { iri =>
              datatypes.byIri += iri -> dataType
            }

//            def store(ct: ClassType[_]): Node = nodes.hasIri(ct.iri).headOption.getOrElse {
//              MemGraphDefault.ns.nodes.hasIri(ct.iri).headOption.getOrElse(classtypes.store(ct))
//            }

            dataType match {
              case dataType: CollectionType[_] =>
                dataType match {
                  case dataType: ListSetType[Any] =>
                    _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                  case dataType: ListType[Any] =>
                    _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                  case dataType: MapType[Any, Any] =>
                    _createEdge(node, MapType.keys.keyRange, ListType(dataType.keyRange))
                    _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                  case dataType: SetType[Any] =>
                    _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                  case dataType: VectorType[Any] =>
                    _createEdge(node, CollectionType.keys.valueRange, ListType(dataType.valueRange))
                  case dataType: Tuple2Type[Any, Any] =>
                    _createEdge(node, TupleType.keys._1stRange, ListType(dataType._1stRange))
                    _createEdge(node, TupleType.keys._2ndRange, ListType(dataType._2ndRange))
                  case dataType: Tuple3Type[Any, Any, Any] =>
                    _createEdge(node, TupleType.keys._1stRange, ListType(dataType._1stRange))
                    _createEdge(node, TupleType.keys._2ndRange, ListType(dataType._2ndRange))
                    _createEdge(node, TupleType.keys._3rdRange, ListType(dataType._3rdRange))
                  case dataType: Tuple4Type[Any, Any, Any, Any] =>
                    _createEdge(node, TupleType.keys._1stRange, ListType(dataType._1stRange))
                    _createEdge(node, TupleType.keys._2ndRange, ListType(dataType._2ndRange))
                    _createEdge(node, TupleType.keys._3rdRange, ListType(dataType._3rdRange))
                    _createEdge(node, TupleType.keys._4rdRange, ListType(dataType._4rdRange))
                  case _ => dataType
                }
              case dataType: DataType[_] =>
              case _ =>
                throw new Exception(s"datatype not found?! ${dataType.iri}")
            }
            dataType.label.foreach {
              case (language, label) =>
                node.addOut(Property.default.`@label`, label).addOut(Property.default.`@language`, language)
            }
            dataType.comment.foreach {
              case (language, comment) =>
                node.addOut(Property.default.`@comment`, comment).addOut(Property.default.`@language`, language)
            }
            dataType.properties.foreach(_createEdge(node, Property.default.`@properties`, _))

            dataType.extendedClasses.foreach(_createEdge(node, Property.default.`@extends`, _))

            node
          }
      }
    }
  }
  val datatypes = new Datatypes {}

  def _createEdge(resource: Resource[_], key: Property, ct: ClassType[_]): Unit = {
    edges.create(
      resource,
      key,
      nodes.hasIri(ct.iri).headOption.getOrElse {
        MemGraphDefault.ns.nodes.hasIri(ct.iri).headOption.getOrElse(classtypes.store(ct))
      }
    )
  }

}
