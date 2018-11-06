package lspace.librarian.provider.mem

import java.util.concurrent.atomic.AtomicLong

import lspace.NS
import lspace.librarian.structure.Property.default
import lspace.librarian.structure._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

object MemResource {}

trait MemResource[T] extends Resource[T] {
  def graph: MemGraph
  @transient lazy val id: Long = graph.IdGenerator.next

  override def iri: String =
    linksOut
      .get(default.iri)
      .flatMap(_.headOption)
      .map(_.inV.value.asInstanceOf[String])
      .getOrElse("")

  override def iris: Set[String] =
    linksOut.get(default.iri).map(_.map(_.inV.value.asInstanceOf[String]).toSet).getOrElse(Set()) ++
      linksOut.get(default.iris).map(_.map(_.inV.value.asInstanceOf[String]).toSet).getOrElse(Set())

  protected[librarian] val linksOut
    : mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]] = //what about mutable.LinkedHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]]
    mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[T, _]]]()

  protected[librarian] val linksIn: mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[_, T]]] =
    mutable.OpenHashMap[Property, mutable.LinkedHashSet[Edge[_, T]]]()

  //  override def keys: Set[Property] = linksOut.keySet ++ linksIn.keySet toSet

  def out(key: Property*): List[Any] =
    if (key.nonEmpty) key.flatMap(key => linksOut.getOrElse(key, List())).map(_.inV.value).toList
    else linksOut.values.flatten.map(_.inV.value).toList

  def outMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.toList.map(_.inV.value))
    else outE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.inV.value))
  }

  def outE(key: Property*): List[Edge[T, Any]] =
    if (key.nonEmpty) key.flatMap(key => linksOut.getOrElse(key, List())).toList
    else linksOut.values.flatten.toList

  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]] = {
    if (key.isEmpty) linksOut.toMap.mapValues(_.toList)
    else outE(key.toList: _*).groupBy(_.key)
  }

  def in(key: Property*): List[Any] =
    if (key.nonEmpty) key.flatMap(key => linksIn.getOrElse(key, List())).map(_.outV.value).toList
    else linksIn.values.flatten.map(_.outV.value).toList

  def inMap(key: Property*): Map[Property, List[Any]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.toList.map(_.outV.value))
    else inE(key.toList: _*).groupBy(_.key).mapValues(_.map(_.outV.value))
  }

  def inE(key: Property*): List[Edge[Any, T]] =
    if (key.nonEmpty) key.flatMap(key => linksIn.getOrElse(key, List())).toList
    else linksIn.values.flatten.toList

  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]] = {
    if (key.isEmpty) linksIn.toMap.mapValues(_.toList)
    else inE(key.toList: _*).groupBy(_.key)
  }

  private def validateDT[V](dt: DataType[V], value: V) =
    if (dt.iri.nonEmpty) dt else ClassType.valueToOntologyResource(value)

//  def addOuts[V, R <: ClassType[V]](key: Property, values: List[(R, V)]): List[Edge[T, V]] =
//    this.synchronized {
//      if (key == null)
//        throw new Exception(s"key == null for property on $iri with value ${values.mkString(" and ")}")
//      graph.ns.getProperty(key.iri).orElse(MemGraphDefault.ns.getProperty(key.iri)).getOrElse {
//        if (!Graph.reservedKeys.contains(key)) Property(graph.ns.storeProperty(key))
//      }
//      //    if (values.exists {
//      //      _ match {
//      //        case resource: Resource[_] if resource.graph != graph => true
//      //        case _ => false
//      //      }
//      //    }) 1 //TODO warn user edge is created to remote node, only a ref is stored, not the object itself
//
//      if (values.nonEmpty)
//        key.container match {
//          case Some(container) =>
//            container match {
//              case NS.types.list | NS.types.language | NS.types.index =>
//                val newProperties = values
//                  .map {
//                    case (clsType: ClassType[_], r: Resource[_])  => graph.upsertResource(r)
//                    case (clsType: ClassType[_], c: ClassType[_]) => graph.ns.storeClassType(c)
//                    case (datatype: DataType[V], value) =>
//                      graph.createValue(value)(validateDT(datatype, value)) //Try { ClassType.valueToOntologyResource(value) }.toOption.getOrElse(datatype)) //datatype)
//                    case (clsType: ClassType[_], value) =>
//                      graph.createValue(value)(ClassType.valueToOntologyResource(value))
//                  }
//                  .map { resource =>
//                    graph.createEdge(this, key, resource.asInstanceOf[MemResource[V]])
//                  }
//
//                newProperties.asInstanceOf[List[MemEdge[T, V]]]
//              case NS.types.set =>
//                val existingProperties = linksOut.getOrElse(key, List()).toList
//                val (edges, upsertedProperties) =
//                  values.distinct.foldLeft(existingProperties -> List[MemEdge[T, V]]()) {
//                    case ((edges, upsertedProperties), (clsType: ClassType[V], value)) =>
//                      (value match {
//                        case iriResource: IriResource if iriResource.iri.nonEmpty =>
//                          edges.find(_.inV.iri == iriResource.iri)
//                        case resource: Resource[_] => edges.find(_.inV.value == resource.value)
//                        case value                 => edges.find(_.inV.value == value)
//                      }).fold {
//                        val resource = clsType -> value match {
//                          case (clsType: ClassType[_], r: Resource[_])  => graph.upsertResource(r)
//                          case (clsType: ClassType[_], c: ClassType[_]) => graph.ns.storeClassType(c)
//                          case (datatype: DataType[V], value) =>
//                            graph.createValue(value)(validateDT(datatype, value)) //Try { ClassType.valueToOntologyResource(value) }.toOption.getOrElse(datatype)) //datatype)
//                          case (clsType: ClassType[_], value) =>
//                            graph.createValue(value)(ClassType.valueToOntologyResource(value))
//                        }
//                        val newEdge = graph.createEdge(this, key, resource.asInstanceOf[MemResource[V]])
//                        (edges :+ newEdge) -> (upsertedProperties :+ newEdge)
//                          .asInstanceOf[List[MemEdge[T, V]]]
//                      } { property =>
//                        edges -> (upsertedProperties :+ property.asInstanceOf[MemEdge[T, V]])
//                      }
//                  }
//                upsertedProperties
//              case NS.types.listset =>
//                val existingProperties = linksOut.getOrElse(key, List()).toList
//                val (edges, upsertedProperties) =
//                  values.distinct.foldLeft(existingProperties -> List[MemEdge[T, V]]()) {
//                    case ((edges, upsertedProperties), (clsType: ClassType[V], value)) =>
//                      (value match {
//                        case iriResource: IriResource if iriResource.iri.nonEmpty =>
//                          edges.find(_.inV.iri == iriResource.iri)
//                        case resource: Resource[_] => edges.find(_.inV.value == resource.value)
//                        case value                 => edges.find(_.inV.value == value)
//                      }).fold {
//                        val resource = clsType -> value match {
//                          case (clsType: ClassType[_], r: Resource[_])  => graph.upsertResource(r)
//                          case (clsType: ClassType[_], c: ClassType[_]) => graph.ns.storeClassType(c)
//                          case (datatype: DataType[V], value) =>
//                            graph.createValue(value)(validateDT(datatype, value)) //Try { ClassType.valueToOntologyResource(value) }.toOption.getOrElse(datatype)) //datatype)
//                          case (clsType: ClassType[_], value) =>
//                            graph.createValue(value)(ClassType.valueToOntologyResource(value))
//                        }
//                        val newEdge = graph.createEdge(this, key, resource.asInstanceOf[MemResource[V]])
//                        (edges :+ newEdge) -> (upsertedProperties :+ newEdge)
//                          .asInstanceOf[List[MemEdge[T, V]]]
//                      } { property =>
//                        edges -> (upsertedProperties :+ property.asInstanceOf[MemEdge[T, V]])
//                      }
//                  }
//                upsertedProperties
//              case NS.types.single =>
//                val existingEdges = linksOut.getOrElse(key, List())
//                //              if (existingProperties.lengthCompare(1) > 0) println(s"cardinality for ${key.iri} should be single but has more than one value")
//                List(
//                  existingEdges.headOption
//                    .filter(existingProperty =>
//                      values.last._2 match {
//                        case iriResource: IriResource if iriResource.iri.nonEmpty =>
//                          existingProperty.inV.iri == iriResource.iri
//                        case resource: Resource[_] => existingProperty.inV.value == resource.value
//                        case value                 => existingProperty.inV.value == value
//                    })
//                    .getOrElse {
//                      existingEdges.headOption.foreach(_.remove())
//                      val resource = values.last match {
//                        case (clsType: ClassType[_], r: Resource[_])  => graph.upsertResource(r)
//                        case (clsType: ClassType[_], c: ClassType[_]) => graph.ns.storeClassType(c)
//                        case (datatype: DataType[V], value) =>
//                          graph.createValue(value)(validateDT(datatype, value)) //Try { ClassType.valueToOntologyResource(value) }.toOption.getOrElse(datatype)) //datatype)
//                        case (clsType: ClassType[_], value) =>
//                          graph.createValue(value)(ClassType.valueToOntologyResource(value))
//                      }
//                      val newEdge = graph.createEdge(this, key, resource.asInstanceOf[MemResource[V]])
//                      newEdge
//                    }).asInstanceOf[List[MemEdge[T, V]]]
//              //TODO: remove removedProperties
//              case _ => throw new Exception(s"unknown container ${container}")
//            }
//          case None =>
//            val existingEdges = linksOut.getOrElse(key, List())
//            //          if (existingProperties.lengthCompare(1) > 0) println(s"cardinality for ${key.iri} should be single but has more than one value")
//            List(
//              existingEdges.headOption
//                .filter(existingProperty =>
//                  values.last._2 match {
//                    case iriResource: IriResource if iriResource.iri.nonEmpty =>
//                      existingProperty.inV.iri == iriResource.iri
//                    case resource: Resource[_] => existingProperty.inV.value == resource.value
//                    case value                 => existingProperty.inV.value == value
//                })
//                .getOrElse {
//                  existingEdges.headOption.foreach(_.remove())
//                  val resource = values.last match {
//                    case (clsType: ClassType[_], r: Resource[_])  => graph.upsertResource(r)
//                    case (clsType: ClassType[_], c: ClassType[_]) => graph.ns.storeClassType(c)
//                    case (datatype: DataType[V], value) =>
//                      graph.createValue(value)(validateDT(datatype, value)) //Try { ClassType.valueToOntologyResource(value) }.toOption.getOrElse(datatype)) //datatype)
//                    case (clsType: ClassType[_], value) =>
//                      graph.createValue(value)(ClassType.valueToOntologyResource(value))
//                  }
//                  val newEdge = graph.createEdge(this, key, resource.asInstanceOf[MemResource[V]])
//                  newEdge
//                }).asInstanceOf[List[MemEdge[T, V]]]
//        } else List()
//    }

  def addIns[V](key: Property, values: List[(ClassType[V], V)]): List[Edge[V, T]] =
    synchronized { //TODO: or linksOut.synchronized ???
      if (key == null)
        throw new Exception(s"key == null for property on $iri with value ${values.mkString(" and ")}")
      //    val key = if (_key.graph == graph) _key else graph.getPropertyKey(_key.iri).getOrElse(graph.storePropertyKey(_key))

      val thisDT = this match {
        case node: Node       => DataType.default.nodeURLType
        case edge: Edge[_, _] => DataType.default.edgeURLType
        case value: Value[_]  => value.label
      }
      values
        .map {
          case (clsType: ClassType[_], r: Resource[_]) => graph.upsertResource(r)
          case (datatype: DataType[V], value) =>
            graph.createValue(value)(validateDT(datatype, value)) //Try { ClassType.valueToOntologyResource(value) }.toOption.getOrElse(datatype)) //datatype)
          case (clsType: ClassType[_], value) =>
            graph.createValue(value)(ClassType.valueToOntologyResource(value))
        }
        .map(_.addOut(key, thisDT.asInstanceOf[ClassType[T]], this.value))
        .asInstanceOf[List[MemEdge[V, T]]]
    }

  def removeInE(edge: Edge[_, _]): Unit = {
    val properties         = linksIn.getOrElse(edge.key, List())
    val (toRemove, toKeep) = properties.partition(_ == edge)
    linksIn += edge.key -> (mutable.LinkedHashSet[Edge[_, T]]() ++ toKeep)
    toRemove.foreach(_.remove())
  }
  def removeOutE(edge: Edge[_, _]): Unit = {
    val properties         = linksOut.getOrElse(edge.key, List())
    val (toRemove, toKeep) = properties.partition(_ == edge)
    linksOut += edge.key -> (mutable.LinkedHashSet[Edge[T, _]]() ++ toKeep)
    toRemove.foreach(_.remove())
  }
  def removeInE(key: Property): Unit = {
    val toRemove = inE(key)
    linksIn -= key
    toRemove.foreach(_.remove())
  }
  def removeOutE(key: Property): Unit = {
    val toRemove = outE(key)
    linksOut -= key
    toRemove.foreach(_.remove())
  }

  protected[mem] def _remove(): Unit = {
    linksOut.foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.to.removeInE(edge))
    }
    linksIn.foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.from.removeOutE(edge))
    }
  }
}
