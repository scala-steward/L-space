package lspace.librarian.structure

import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Property.default
import lspace.util.CacheStatus

object Resource {
  implicit def default[T]: ClassTypeable.Aux[Resource[T], T, IriType[T]] = new ClassTypeable[Resource[T]] {
    type C  = T
    type CT = IriType[T]
    def ct: CT = IriType[T]
  }
}

trait Resource[+T] extends IriResource {
//  @transient
  def id: Long

  /**
    * Get the graph that this resource is within.
    * @return
    */
  implicit def graph: Graph

  def value: T
  def self: Resource[T] = this

  def `@id`: String = iri
  def iri: String = {
    out(default.`@id`).collectFirst { case url: String => url }.getOrElse("")
  }
  def `@ids`: Set[String] = iris
  def iris: Set[String]   = out(default.`@ids`).collect { case url: String => url }.toSet

  @transient var status: CacheStatus.CacheStatus = CacheStatus.EMPTY
  @transient var memento: Long                   = 0L

  //  def keys: Set[Property] = outE().map(_.key).toSet ++ inE().map(_.key).toSet
  def `@type`: List[ClassType[_]] = labels
  def labels: List[ClassType[_]]

  //  def start(): Traversal[Resource[T], Resource[T], R :: HNil, HNil] = Traversal[Resource[T], Resource[T], HNil]()(graph, LabelsHList(HNil)).R[T](this)

  def sameResource(resource: Resource[_]): Boolean = resource.id == id && resource.graph == graph

  override def equals(o: scala.Any): Boolean = o match {
    case resource: Resource[_] => sameResource(resource)
    case _                     => false
  }
  def hashcode: Int = id.hashCode()

  override def hashCode(): Int = hashcode

  def hasLabel[L](label: ClassType[L]*): Option[Resource[L]] = {
    if (labels.exists(tpe => label.contains(tpe)) || {
          val extendedTypes = labels //.map(ClassType.wrap)
          //      println(s"haslabel ${extendedTypes.map(_.iri)} ${extendedTypes.exists(_.`extends`(label.head))}")
          label.exists(range => extendedTypes.exists(_.`extends`(range)))
        }) Some(this.asInstanceOf[Resource[L]])
    else None
  }
  //  implicit def iriToPropertyKey(iri: String) = Property(iri)
  def out(key: String, keys: String*): List[Any] =
    out(Property(key) :: keys.toList.map(Property(_)): _*)
  def out(key: Property*): List[Any]
  def outMap(key: String, keys: String*): Map[Property, List[Any]] =
    outMap(Property(key) :: keys.toList.map(Property(_)): _*)
  def outMap(key: Property*): Map[Property, List[Any]]
  def outE(key: String, keys: String*): List[Edge[T, Any]] =
    outE(Property(key) :: keys.toList.map(Property(_)): _*)
  def outE(key: Property*): List[Edge[T, Any]]
  def outEMap(key: String, keys: String*): Map[Property, List[Edge[T, Any]]] =
    outEMap(Property(key) :: keys.toList.map(Property(_)): _*)
  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]]
  def out[V](key: TypedProperty[V], keys: TypedProperty[V]*): List[V] =
    outE(key.key).flatMap(_.inV.hasLabel(key.range).map(_.value)) ++ keys.flatMap(key =>
      outE(key.key).flatMap(_.inV.hasLabel(key.range).map(_.value)))
  def outE[V](key: TypedProperty[V], keys: TypedProperty[V]*): List[Edge[T, V]] =
    outE(key.key).filter(_.inV.hasLabel(key.range).isDefined).asInstanceOf[List[Edge[T, V]]] ++
      keys
        .flatMap(key => outE(key.key).filter(_.inV.hasLabel(key.range).isDefined))
        .toList
        .asInstanceOf[List[Edge[T, V]]]
  def in(key: String, keys: String*): List[Any] =
    in(Property(key) :: keys.toList.map(Property(_)): _*)
  def in(key: Property*): List[Any]
  def inMap(key: String, keys: String*): Map[Property, List[Any]] =
    inMap(Property(key) :: keys.toList.map(Property(_)): _*)
  def inMap(key: Property*): Map[Property, List[Any]]
  def inE(key: String, keys: String*): List[Edge[Any, T]] =
    inE(Property(key) :: keys.toList.map(Property(_)): _*)
  def inE(key: Property*): List[Edge[Any, T]]
  def inEMap(key: String, keys: String*): Map[Property, List[Edge[Any, T]]] =
    inEMap(Property(key) :: keys.toList.map(Property(_)): _*)
  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]]

  def ---(key: String): PartialOutEdge[T] =
    ---(
      graph.ns
        .getProperty(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ))
  def ---(key: Property): PartialOutEdge[T] = PartialOutEdge(this, key)
  def ---(f: Property.default.type => Property): PartialOutEdge[T] =
    PartialOutEdge(this, f(Property.default))

  /**
    * Edge with Cardinality single
    * @param key
    * @return
    */
  def -|-(key: Property): PartialOutEdge[T] = ???

  /**
    * Edge with Cardinality list
    * @param key
    * @return
    */
  def -*-(key: Property): PartialOutEdge[T] = ???
  import shapeless.<:!<
  def addOut[V, V0, VT0 <: ClassType[_]](key: String, value: V)(implicit ev1: V <:!< ClassType[_],
                                                                dt: ClassTypeable.Aux[V, V0, VT0]): Edge[T, V0] =
    addOut[V, V0, VT0](
      graph.ns
        .getProperty(key)
        .orElse(MemGraphDefault.ns.getProperty(key))
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addOut[V <: ClassType[_]](key: String, value: V): Edge[T, Node] =
    addOut(
      graph.ns
        .getProperty(key)
        .orElse(MemGraphDefault.ns.getProperty(key))
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addOut[V, V0, VT0 <: ClassType[_]](key: Property, value: V)(implicit ev1: V <:!< ClassType[_],
                                                                  dt: ClassTypeable.Aux[V, V0, VT0]): Edge[T, V0] = {
    val toResource = value match {
      case resource: Resource[_] => graph.resources.upsert(resource)
      case _ =>
        dt.ct match {
          case dt: DataType[V] =>
            graph.values.create(value, dt)
          case ct: ClassType[V] =>
            graph.values.create(value)
        }
    }
    graph.edges.create(this, key, toResource.asInstanceOf[Resource[V0]])
  }

//    addOuts(key, dt.ct -> value.asInstanceOf[V0] :: Nil).head
  def addOut[V, R[Z] <: ClassType[Z]](key: Property, dt: R[V], value: V)(
      implicit ev1: V <:!< ClassType[_]): Edge[T, V] = {
    val toResource = value match {
      case resource: Resource[V] => resource.asInstanceOf[Resource[V]]
      case _ =>
        dt match {
          case dt: DataType[V] =>
            graph.values.create(value, dt)
          case ct: ClassType[V] =>
            graph.values.create[V](value)
        }
    }
    graph.edges.create(this, key, toResource)
  }

//    addOuts(key, List(dt -> value)).head
  def addOut[V <: ClassType[_]](key: Property, value: V): Edge[T, Node] = {
    val toResource = graph.ns.storeClassType(value)
//    val toResource = graph.nodes.upsert(value.iri, value.iris)
//    value match {
//      case ontology: Ontology => toResource.addLabel(Ontology.ontology)
//      case property: Property => toResource.addLabel(Property.ontology)
//      case datatype: DataType[_] =>
//        toResource.addLabel(Ontology.ontology)
//        toResource.addLabel(DataType.ontology)
//    }
    graph.edges.create(this, key, toResource)
  }
//    addOuts(key, List(IriType[V] -> value)).head
  def addOut[V](key: TypedProperty[V], value: V): Edge[T, V] = {
    val toResource = value match {
      case resource: Resource[V] => graph.resources.upsert(resource).asInstanceOf[Resource[V]]
      case _                     => graph.values.create(value, key.range.asInstanceOf[DataType[V]])
    }
    graph.edges.create(this, key.key, toResource)
  }

  def <--(key: String): PartialInEdge[T] =
    <--(
      graph.ns
        .getProperty(key)
        .orElse(MemGraphDefault.ns.getProperty(key))
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ))
  def <--(key: Property): PartialInEdge[T] = PartialInEdge(this, key)
  def addIn[V, V0, VT0 <: ClassType[_]](key: String, value: V)(implicit ev1: V <:!< ClassType[_],
                                                               dt: ClassTypeable.Aux[V, V0, VT0]): Edge[V0, T] =
    addIn[V, V0, VT0](
      graph.ns
        .getProperty(key)
        .orElse(MemGraphDefault.ns.getProperty(key))
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addIn[V <: ClassType[_]](key: String, value: V): Edge[Node, T] =
    addIn(
      graph.ns
        .getProperty(key)
        .orElse(MemGraphDefault.ns.getProperty(key))
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addIn[V, V0, VT0 <: ClassType[_]](key: Property, value: V)(implicit ev1: V <:!< ClassType[_],
                                                                 dt: ClassTypeable.Aux[V, V0, VT0]): Edge[V0, T] = {
    val toResource = value match {
      case resource: Resource[_] => graph.resources.upsert(resource)
      case _ =>
        dt.ct match {
          case dt: DataType[V] =>
            graph.values.create(value, dt)
          case ct: ClassType[V] =>
            graph.values.create(value)
        }
    }
    graph.edges.create(toResource.asInstanceOf[Resource[V0]], key, this)
  }
  def addIn[V, R[Z] <: ClassType[Z]](key: Property, dt: R[V], value: V)(
      implicit ev1: V <:!< ClassType[_]): Edge[V, T] = {
    val toResource = value match {
      case resource: Resource[V] => graph.resources.upsert(resource).asInstanceOf[Resource[V]]
      case _ =>
        dt match {
          case dt: DataType[V] =>
            graph.values.create(value, dt)
          case ct: ClassType[V] =>
            graph.values.create[V](value)
        }
    }
    graph.edges.create(toResource, key, this)
  }

  def addIn[V <: ClassType[_]](key: Property, value: V): Edge[Node, T] = {
    val fromResource = graph.ns.storeClassType(value)
//    val fromResource = graph.nodes.upsert(value.iri, value.iris)
//    value match {
//      case ontology: Ontology => fromResource.addLabel(Ontology.ontology)
//      case property: Property => fromResource.addLabel(Property.ontology)
//      case datatype: DataType[_] =>
//        fromResource.addLabel(Ontology.ontology)
//        fromResource.addLabel(DataType.ontology)
//    }
    graph.edges.create(fromResource, key, this)
  }

  def addBoth[V, R[T] <: Resource[T]](key: Property, value: R[V]): (Edge[T, V], Edge[V, T]) = {
    val fromCT = this match {
      case node: Node       => DataType.default.nodeURLType
      case edge: Edge[_, _] => DataType.default.edgeURLType
      case value: Value[_]  => value.label
    }
    val valueCT = value match {
      case node: Node       => DataType.default.nodeURLType
      case edge: Edge[_, _] => DataType.default.edgeURLType
      case value: Value[_]  => value.label
    }

    addOut(key, valueCT.asInstanceOf[ClassType[R[V]]], value)
      .asInstanceOf[Edge[T, V]] -> value
      .addOut(key, fromCT.asInstanceOf[ClassType[Resource[T]]], this.asInstanceOf[Resource[T]])
      .asInstanceOf[Edge[V, T]]
  }

  def removeInE(edge: Edge[_, _]): Unit
  def removeOutE(edge: Edge[_, _]): Unit
  def removeInE(key: Property): Unit
  def removeOutE(key: Property): Unit

  def remove(): Unit
}
