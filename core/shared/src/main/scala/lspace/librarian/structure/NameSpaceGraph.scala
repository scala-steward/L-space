package lspace.librarian.structure

import lspace.NS
import lspace.librarian.datatype._
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Property.default
import lspace.util.types.DefaultsToAny

import scala.collection.mutable

trait NameSpaceGraph extends Graph {
  def ns: NameSpaceGraph = this
  def graph: Graph
  protected[librarian] val ontologies: mutable.HashMap[String, Ontology] =
    mutable.HashMap[String, Ontology]()
  protected[librarian] val properties: mutable.HashMap[String, Property] =
    mutable.HashMap[String, Property]()
  protected[librarian] val datatypes: mutable.HashMap[String, DataType[_]] =
    mutable.HashMap[String, DataType[_]]()

  def getOntologies(iri: String*): List[Ontology] = {
    if (iri.isEmpty) ns.ontologies.values.toList
    else iri.toList.flatMap(ns.ontologies.get)
  }

  def getOntology(iri: String): Option[Ontology] = {
    ns.ontologies
      .get(iri)
      .orElse(MemGraphDefault.ns.ontologies.get(iri))
      .orElse {
        ns.getNode(iri).headOption.collect {
          case node if node.labels.contains(Ontology.ontology) && !node.labels.contains(DataType.ontology) =>
            val ontology = Ontology(node)
            ns.ontologies += iri -> ontology
            if (graph != MemGraphDefault) MemGraphDefault.ns.storeOntology(ontology)
            ontology
          //          case _ => throw new Exception(s"Cannot produce Ontology from node with iri: ${iri}") //return None?
        }
      }
  }

  def storeOntology(ontology: Ontology): Node = {
    if (graph != MemGraphDefault) MemGraphDefault.ns.storeOntology(ontology)

    ns.getNode(ontology.iri).headOption.filter(_.labels.contains(Ontology.ontology)).getOrElse {
      val node = ns.upsertNode(ontology.iri, ontology.iris)
      node.addLabel(Ontology.ontology)
      ontology.label.foreach {
        case (language, label) =>
          node.addOut(Property.default.label, label).addOut(Property.default.language, language)
      }
      ontology.comment.foreach {
        case (language, comment) =>
          node.addOut(Property.default.comment, comment).addOut(Property.default.language, language)
      }
      ontology.properties.foreach { property =>
        node.addOut(
          Property.default.properties,
          ns.getNode(property.iri).headOption.getOrElse {
            storeProperty(property)
            ns.getNode(property.iri)
              .headOption
              .getOrElse(throw new Exception(s"Property ${property.iri} stored but could not be retrieved"))
          }
        )
      }
      ontology.extendedClasses.foreach { ontology =>
        node.addOut(
          Property.default.EXTENDS,
          ns.getNode(ontology.iri).headOption.getOrElse {
            storeOntology(ontology)
            ns.getNode(ontology.iri)
              .headOption
              .getOrElse(throw new Exception(s"Ontology ${ontology.iri} stored but could not be retrieved"))
          }
        )
      }
      ontology.base.foreach { base =>
        node.addOut(Property.default.base, base)
      }
      ns.ontologies += ontology.iri -> ontology
      node
    }
  }

  def getProperties(iri: String*): List[Property] = {
    if (iri.isEmpty) ns.properties.values.toList
    else iri.toList.flatMap(ns.properties.get)
  }

  def getProperty(iri: String): Option[Property] = {
    ns.properties
      .get(iri)
      .orElse(MemGraphDefault.ns.properties.get(iri))
      .orElse {
        ns.getNode(iri).headOption.collect {
          case node if node.labels.contains(Property.ontology) =>
            val range = () =>
              node.out(Property.default.typed.rangeDataType).map { dataTypeNode =>
                dataTypeNode.iri match {
                  case NS.types.string   => DataType.default.textType
                  case NS.types.int      => DataType.default.intType
                  case NS.types.double   => DataType.default.doubleType
                  case NS.types.long     => DataType.default.longType
                  case NS.types.date     => DataType.default.dateType
                  case NS.types.datetime => DataType.default.dateTimeType
                  case NS.types.time     => DataType.default.timeType
                  //              case ldcontext.types.epochtime => epochType
                  case NS.types.boolean   => DataType.default.boolType
                  case NS.types.geo       => DataType.default.geoType
                  case NS.types.geopoint  => DataType.default.geopointType
                  case NS.types.schemaURL => DataType.default.uRLType
                  case NS.types.color     => DataType.default.textType
                  case iri =>
                    getOntology(iri)
                      .orElse(throw new Exception(s"uncached ontology $iri"))
                      .get
                }
              //                NS.typed += dataTypeKey.iri -> dataTypeKey
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
            val extendedClasses = () => node.out(default.typed.extendsProperty).map(Property.apply)
            val properties      = () => node.out(default.typed.propertyProperty).map(Property.apply)
            val base            = node.out(default.typed.baseString).headOption

            val property =
              Property(node.iri)(node.iris, range, containers, label, comment, extendedClasses, properties, base)

            if (graph != MemGraphDefault && MemGraphDefault.ns.getProperty(node.iri).isEmpty) {
              MemGraphDefault.ns.storeProperty(property)
            }

            ns.properties += iri -> property
            property
          //          case _ => throw new Exception(s"Cannot produce Property from node with iri: ${iri}")
        }
      }
  }

  def storeProperty(property: Property): Node = {
    if (graph != MemGraphDefault) MemGraphDefault.ns.storeProperty(property)

    ns.getNode(property.iri).headOption.filter(_.labels.contains(DataType.ontology)).getOrElse {
      val node = ns.upsertNode(property.iri, property.iris)
      node.addLabel(Property.ontology)
      property.range.foreach { range =>
        node.addOut(
          Property.default.range,
          ns.getNode(range.iri).headOption.getOrElse {
            range match {
              case dt: DataType[_] => storeDataType(dt)
              case o: Ontology     => storeOntology(o)
              case p: Property     => storeProperty(p)
              case c: ClassType[_] =>
                println(c.iri)
                throw new Exception(s"could not match ${c.iri} for property ${property.iri}")
            }
            ns.getNode(range.iri)
              .headOption
              .getOrElse(throw new Exception(s"ClassType ${range.iri} stored but could not be retrieved"))
          }
        )
      }
      property.containers.foreach { container =>
        node.addOut(default.container, container)
      }
      property.label.foreach {
        case (language, label) =>
          node.addOut(Property.default.label, label).addOut(Property.default.language, language)
      }
      property.comment.foreach {
        case (language, comment) =>
          node.addOut(Property.default.comment, comment).addOut(Property.default.language, language)
      }
      property.properties.foreach { property =>
        node.addOut(
          Property.default.properties,
          ns.getNode(property.iri).headOption.getOrElse {
            storeProperty(property)
            ns.getNode(property.iri)
              .headOption
              .getOrElse(throw new Exception(s"Property ${property.iri} stored but could not be retrieved"))
          }
        )
      }
      property.extendedClasses.foreach { property =>
        node.addOut(
          Property.default.EXTENDS,
          ns.getNode(property.iri).headOption.getOrElse {
            storeProperty(property)
            ns.getNode(property.iri)
              .headOption
              .getOrElse(throw new Exception(s"Property ${property.iri} stored but could not be retrieved"))
          }
        )
      }
      property.base.foreach { base =>
        node.addOut(Property.default.base, base)
      }
      ns.properties += property.iri -> property
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
    ns.datatypes
      .get(iri)
      .orElse {
        Option(iri match {
          case NS.types.string         => DataType.default.textType
          case NS.types.schemaText     => DataType.default.textType
          case NS.types.int            => DataType.default.intType
          case NS.types.schemaInteger  => DataType.default.intType
          case NS.types.double         => DataType.default.doubleType
          case NS.types.schemaFloat    => DataType.default.doubleType
          case NS.types.long           => DataType.default.longType
          case NS.types.datetime       => DataType.default.dateTimeType
          case NS.types.schemaDateTime => DataType.default.dateTimeType
          case NS.types.date           => DataType.default.dateType
          case NS.types.schemaDate     => DataType.default.dateType
          case NS.types.time           => DataType.default.timeType
          //    case ldcontext.types.schemaTime => timeType,
          //    case ldcontext.types.epochtime => epochType
          case NS.types.boolean       => DataType.default.boolType
          case NS.types.schemaBoolean => DataType.default.boolType
          case NS.types.geojson       => DataType.default.geoType
          case NS.types.geopoint      => DataType.default.geopointType
          case NS.types.id            => DataType.default.uRLType
          case NS.types.reverse       => DataType.default.uRLType
          case NS.types.schemaURL     => DataType.default.uRLType
          case _                      => null
        })
      }
      .asInstanceOf[Option[DataType[T]]]
  }

  def storeDataType(dataType: DataType[_]): Node = {
    ns.getNode(dataType.iri).headOption.getOrElse {
      val node = ns.upsertNode(dataType.iri, dataType.iris)
      node.addLabel(Ontology.ontology)
      node.addLabel(DataType.ontology)
      dataType match {
        case dataType: CollectionType[_] =>
          dataType match {
            case dataType: ListSetType[Any] =>
              dataType.valueRange
                .map(storeClassType)
                .foreach(node => node.addOut(CollectionType.keys.valueRange, node))
            case dataType: ListType[Any] =>
              dataType.valueRange
                .map(storeClassType)
                .foreach(node => node.addOut(CollectionType.keys.valueRange, node))
            case dataType: MapType[Any, Any] =>
              dataType.keyRange
                .map(storeClassType)
                .foreach(node => node.addOut(MapType.keys.keyRange, node))
              dataType.valueRange
                .map(storeClassType)
                .foreach(node => node.addOut(CollectionType.keys.valueRange, node))
            case dataType: SetType[Any] =>
              dataType.valueRange
                .map(storeClassType)
                .foreach(node => node.addOut(CollectionType.keys.valueRange, node))
            case dataType: VectorType[Any] =>
              dataType.valueRange
                .map(storeClassType)
                .foreach(node => node.addOut(CollectionType.keys.valueRange, node))
            case _ => dataType
          }
        case dataType: DataType[_] =>
        case _ =>
          throw new Exception(s"datatype not found?! ${dataType.iri}")
      }
      dataType.label.foreach {
        case (language, label) =>
          node.addOut(Property.default.label, label).addOut(Property.default.language, language)
      }
      dataType.comment.foreach {
        case (language, comment) =>
          node.addOut(Property.default.comment, comment).addOut(Property.default.language, language)
      }
      dataType.properties.foreach { property =>
        node.addOut(
          Property.default.properties,
          ns.getNode(property.iri).headOption.getOrElse {
            storeProperty(property)
            ns.getNode(property.iri)
              .headOption
              .getOrElse(throw new Exception(s"Property ${property.iri} stored but could not be retrieved"))
          }
        )
      }
      dataType.extendedClasses.foreach { dataType =>
        node.addOut(
          Property.default.EXTENDS,
          ns.getNode(dataType.iri).headOption.getOrElse {
            storeDataType(dataType)
            ns.getNode(dataType.iri)
              .headOption
              .getOrElse(throw new Exception(s"DataType ${dataType.iri} stored but could not be retrieved"))
          }
        )
      }
      ns.datatypes += dataType.iri -> dataType
      node
    }
  }
}
