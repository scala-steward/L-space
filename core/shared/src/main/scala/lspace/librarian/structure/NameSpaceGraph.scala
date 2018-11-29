package lspace.librarian.structure

import lspace.librarian.datatype._
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Property.default
import lspace.librarian.structure.util.IdProvider
import lspace.util.types.DefaultsToAny

import scala.collection.mutable

trait NameSpaceGraph extends DataGraph {
  def ns: NameSpaceGraph = this
  def index: IndexGraph
  def graph: Graph

  lazy val idProvider: IdProvider = graph.idProvider

  override def init(): Unit = {
    index.init()
  }

  protected[librarian] val ontologies = new {
    val byId: mutable.HashMap[Long, Ontology] =
      mutable.HashMap[Long, Ontology]()
    val byIri: mutable.HashMap[String, Ontology] =
      mutable.HashMap[String, Ontology]()
  }
  protected[librarian] val properties = new {
    val byId: mutable.HashMap[Long, Property] =
      mutable.HashMap[Long, Property]()
    val byIri: mutable.HashMap[String, Property] =
      mutable.HashMap[String, Property]()
  }
  protected[librarian] val datatypes = new {
    val byId: mutable.HashMap[Long, DataType[_]] =
      mutable.HashMap[Long, DataType[_]]()
    val byIri: mutable.HashMap[String, DataType[_]] =
      mutable.HashMap[String, DataType[_]]()
  }

  def getOntologies(iri: String*): List[Ontology] = {
    if (iri.isEmpty) ns.ontologies.byIri.values.toList
    else iri.toList.flatMap(getOntology)
  }

  def getOntologies(id: Long, ids: Long*): List[Ontology] = {
    (id :: ids.toList).map(
      id =>
        ontologyFromCache(id)
          .getOrElse(ontologyFromNode(nodeStore.byId(id).head)))
  }

  def getOntology(iri: String): Option[Ontology] = {
    ontologyFromCache(iri)
      .orElse {
        nodeStore
          .byIri(iri)
          .find(n => n.hasLabel(Ontology.ontology).isDefined && n.hasLabel(DataType.ontology).isEmpty)
          .map(ontologyFromNode)
      }
  }

  protected def ontologyFromCache(id: Long): Option[Ontology] =
    Ontology.allOntologies.byId
      .get(id)
      .orElse(ns.ontologies.byId.get(id))

  protected def ontologyFromCache(iri: String): Option[Ontology] =
    Ontology.allOntologies.byIri
      .get(iri)
      .orElse(ns.ontologies.byIri
        .get(iri))
      .orElse(MemGraphDefault.ns.ontologies.byIri.get(iri))

  private def ontologyFromNode(node: _Node): Ontology = {
    val ontology = Ontology(node)
    ns.ontologies.byId += node.id -> ontology
    ns.ontologies.byIri += iri    -> ontology
    ontology.iris.foreach(iri => ns.ontologies.byIri += iri -> ontology)
    if (graph != MemGraphDefault) MemGraphDefault.ns.storeOntology(ontology)
    ontology
  }

  def storeOntology(ontology: Ontology): Node = {
    if (graph != MemGraphDefault) MemGraphDefault.ns.storeOntology(ontology)
    if (Ontology.allOntologies.byIri.get(ontology.iri).isDefined) {
      val node = nodes.upsert(ontology.iri, ontology.iris)
      node.addLabel(Ontology.ontology)
      node
    } else
      ns.nodes
        .hasIri(ontology.iri)
        .find(n => n.hasLabel(Ontology.ontology).isDefined && n.hasLabel(DataType.ontology).isEmpty)
//      .filter(o => ontology.iris diff o.iris nonEmpty)
        .getOrElse {
//        val node = ns.nodes.upsert(ontology.iri, ontology.iris)
//        node.addLabel(Ontology.ontology)
          val node = Ontology.allOntologies.idByIri
            .get(ontology.iri)
            .map(id => ns.nodes.create(id)(Ontology.ontology))
            .getOrElse(ns.nodes.create(Ontology.ontology))

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
          ns.ontologies.byId += node.id       -> ontology
          ns.ontologies.byIri += ontology.iri -> ontology
          ontology.iris.foreach { iri =>
            ns.ontologies.byIri += iri -> ontology
          }
          node
        }
  }

  def getProperties(iri: String*): List[Property] = {
    if (iri.isEmpty) ns.properties.byIri.values.toList
    else iri.toList.flatMap(getProperty)
  }

  def getProperties(id: Long, ids: Long*): List[Property] = {
    (id :: ids.toList).map(
      id =>
        propertyFromCache(id)
          .getOrElse(propertyFromNode(nodeStore.byId(id).head)))
  }

  protected def propertyFromCache(id: Long): Option[Property] =
    Property.allProperties.byId
      .get(id)
      .orElse(ns.properties.byId.get(id))

  protected def propertyFromCache(iri: String): Option[Property] =
    Property.allProperties.byIri
      .get(iri)
      .orElse(ns.properties.byIri
        .get(iri))
      .orElse(MemGraphDefault.ns.properties.byIri.get(iri))

  def getProperty(iri: String): Option[Property] = {
    propertyFromCache(iri)
      .orElse {
        nodeStore.byIri(iri).find(_.hasLabel(Property.ontology).isDefined).map(propertyFromNode)
      }
  }

  private def propertyFromNode(node: _Node): Property = {
    val range = () =>
      node.out(Property.default.`@range`).collect {
        case node: _Node =>
          if (node.hasLabel(DataType.ontology).isDefined)
            datatypeFromCache(node.iri)
              .getOrElse(datatypeFromNode(node))
          else if (node.hasLabel(Ontology.ontology).isDefined)
            ontologyFromCache(node.iri)
              .getOrElse(ontologyFromNode(node))
          else if (node.hasLabel(Property.ontology).isDefined)
            propertyFromCache(node.iri)
              .getOrElse(propertyFromNode(node))
          else throw new Exception(s"range ${node.iri} is not of @type @class, @property or @datatype")
    }

    val containers = node.out(default.typed.containerString)
    val label = node
      .outE(default.typed.labelString)
      .flatMap { edge =>
        edge.out(default.typed.languageString).map(_ -> edge.to.value)
      }
      .toMap
    val comment = node
      .outE(default.typed.commentString)
      .flatMap { edge =>
        edge.out(default.typed.languageString).map(_ -> edge.to.value)
      }
      .toMap
    val extendedClasses = () =>
      node.out(default.`@extends`).collect {
        case node: _Node => propertyFromCache(node.iri).getOrElse(propertyFromNode(node))
    }
    val properties = () => node.out(default.typed.propertyProperty).map(Property.apply)
    val base       = node.out(default.typed.baseString).headOption

    val property =
      Property(node.iri)(node.iris, range, containers, label, comment, extendedClasses, properties, base)

    if (graph != MemGraphDefault && MemGraphDefault.ns.getProperty(node.iri).isEmpty) {
      MemGraphDefault.ns.storeProperty(property)
    }

    ns.properties.byIri += iri    -> property
    ns.properties.byId += node.id -> property
    property.iris.foreach(iri => ns.properties.byIri += iri -> property)
    property
  }

  def storeProperty(property: Property): Node = {
    if (graph != MemGraphDefault) MemGraphDefault.ns.storeProperty(property)
    if (Property.allProperties.byIri.get(property.iri).isDefined) {
      val node = nodes.upsert(property.iri, property.iris)
      node.addLabel(Property.ontology)
      node
    } else
      ns.nodes.hasIri(property.iri).find(_.hasLabel(Property.ontology).isDefined).getOrElse {
//      val node = ns.nodes.upsert(property.iri, property.iris)
//      node.addLabel(Property.ontology)
        val node = Property.allProperties.idByIri
          .get(property.iri)
          .map(id => ns.nodes.create(id)(Property.ontology))
          .getOrElse(ns.nodes.create(Property.ontology))

        node.addOut(default.typed.iriUrlString, property.iri)
        property.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

        property.range.foreach(_createEdge(node, Property.default.`@range`, _))
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
        ns.properties.byId += node.id       -> property
        ns.properties.byIri += property.iri -> property
        property.iris.foreach { iri =>
          ns.properties.byIri += iri -> property
        }
        node
      }
  }

  def getClassType(iri: String): Option[ClassType[_]] =
    getDataType(iri)
      .orElse(MemGraphDefault.ns.getDataType(iri))
      .orElse(getProperty(iri).orElse(MemGraphDefault.ns.getProperty(iri)))
      .orElse(getOntology(iri).orElse(MemGraphDefault.ns.getOntology(iri)))

  def getClassType(node: Node): ClassType[_] = node.labels match {
    case l if l.contains(DataType.ontology) =>
      getDataType(node.iri)
        .orElse(MemGraphDefault.ns.getDataType(node.iri))
        .getOrElse(DataType.apply(node))
    case l if l.contains(Property.ontology) =>
      getProperty(node.iri)
        .orElse(MemGraphDefault.ns.getProperty(node.iri))
        .getOrElse(Property.apply(node))
    case l if l.contains(Ontology.ontology) =>
      getOntology(node.iri)
        .orElse(MemGraphDefault.ns.getOntology(node.iri))
        .getOrElse(Ontology.apply(node))
    case _ =>
      throw new Exception(s"could not find class-type ${node.iri}")
  }

  def storeClassType[CT <: ClassType[_]](classType: CT): Node = classType match {
    case ontology: Ontology    => storeOntology(ontology)
    case property: Property    => storeProperty(property)
    case datatype: DataType[_] => storeDataType(datatype)
  }

  def getDataType[T: DefaultsToAny](iri: String): Option[DataType[T]] = {
    datatypeFromCache(iri)
      .orElse(nodeStore.byIri(iri).find(_.hasLabel(DataType.ontology).isDefined).map(datatypeFromNode))
      .asInstanceOf[Option[DataType[T]]]
  }

  def getDataTypes[T: DefaultsToAny](id: Long, ids: Long*): List[DataType[T]] = {
    (id :: ids.toList)
      .map(
        id =>
          datatypeFromCache(id)
            .getOrElse(datatypeFromNode(nodeStore.byId(id).head)))
      .asInstanceOf[List[DataType[T]]]
  }

  protected def datatypeFromCache(id: Long): Option[DataType[_]] =
    DataType.allDataTypes.byId
      .get(id)
      .orElse(ns.datatypes.byId.get(id))

  protected def datatypeFromCache(iri: String): Option[DataType[_]] =
    DataType.allDataTypes.byIri
      .get(iri)
      .orElse(ns.datatypes.byIri
        .get(iri))
      .orElse(MemGraphDefault.ns.datatypes.byIri.get(iri))

  private def datatypeFromNode(node: _Node): DataType[_] = ??? //TODO: retrieve custom collection datatypes

  def _createEdge(resource: Resource[_], key: Property, ct: ClassType[_]): Unit = {
    edges.create(
      resource,
      key,
      nodes.hasIri(ct.iri).headOption.getOrElse {
        MemGraphDefault.ns.nodes.hasIri(ct.iri).headOption.getOrElse(storeClassType(ct))
      }
    )
  }

  def storeDataType(dataType: DataType[_]): Node = {
    if (DataType.allDataTypes.byIri.get(dataType.iri).isDefined) {
      val node = nodes.upsert(dataType.iri, dataType.iris)
      node.addLabel(Ontology.ontology) //TODO: redundant? removed by addLabel(DataType.ontology) because DataType extends Ontology
      node.addLabel(DataType.ontology)
      node
    } else
      ns.nodes
        .hasIri(dataType.iri)
        .find(_.hasLabel(DataType.ontology).isDefined)
        .getOrElse {
//      val node = ns.nodes.upsert(dataType.iri, dataType.iris)
          val node = DataType.allDataTypes.idByIri
            .get(dataType.iri)
            .map(id => ns.nodes.create(id)(Ontology.ontology, DataType.ontology))
            .getOrElse(ns.nodes.create(Ontology.ontology, DataType.ontology))
          node.addOut(default.typed.iriUrlString, dataType.iri)
          dataType.iris.foreach(iri => node.addOut(default.typed.irisUrlString, iri))

          def _storeClassType(ct: ClassType[_]): Node = nodes.hasIri(ct.iri).headOption.getOrElse {
            MemGraphDefault.ns.nodes.hasIri(ct.iri).headOption.getOrElse(storeClassType(ct))
          }

          dataType match {
            case dataType: CollectionType[_] =>
              dataType match {
                case dataType: ListSetType[Any] =>
                  dataType.valueRange.foreach(_createEdge(node, CollectionType.keys.valueRange, _))
                case dataType: ListType[Any] =>
                  dataType.valueRange.foreach(_createEdge(node, CollectionType.keys.valueRange, _))
                case dataType: MapType[Any, Any] =>
                  dataType.keyRange.foreach(_createEdge(node, MapType.keys.keyRange, _))
                  dataType.valueRange.foreach(_createEdge(node, CollectionType.keys.valueRange, _))
                case dataType: SetType[Any] =>
                  dataType.valueRange.foreach(_createEdge(node, CollectionType.keys.valueRange, _))
                case dataType: VectorType[Any] =>
                  dataType.valueRange.foreach(_createEdge(node, CollectionType.keys.valueRange, _))
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
          ns.datatypes.byIri += dataType.iri -> dataType
          ns.datatypes.byId += node.id       -> dataType
          dataType.iris.foreach { iri =>
            ns.datatypes.byIri += iri -> dataType
          }
          node
        }
  }
}
