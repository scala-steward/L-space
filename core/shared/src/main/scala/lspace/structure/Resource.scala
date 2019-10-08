package lspace.structure

import lspace.datatype.{DataType, IriType}
import lspace.structure.util.ClassTypeable
import lspace.structure.Property.default
import lspace.util.CacheStatus
import monix.eval.Task

object Resource {
  implicit def default[T]: ClassTypeable.Aux[Resource[T], T, IriType[T]] = new ClassTypeable[Resource[T]] {
    type C  = T
    type CT = IriType[T]
    def ct: CT = IriType[T]
  }
}

trait Resource[+T] extends IriResource {
//  @transient
  /**
    * id is a unique identifier of a resource in a graph
    * @return
    */
  def id: Long

  /**
    * Get the graph that this resource is within.
    * @return
    */
  val graph: Graph

  /**
    * The unboxed value
    * @return
    */
  def value: T
  def self: Resource[T] = this

  /**
    * alias for `@id`
    * @return
    */
  def iri: String = {
    out(default.`@id`).collectFirst { case url: String => url }.getOrElse("")
  }

  /**
    * The @ids are alternative (same-as) IRI/URI identifiers
    * @return a Set[String] which has one or more values (it always includes `@id`)
    */
  def `@ids`: Set[String] = iris

  /**
    * alias for `@ids`
    * @return
    */
  def iris: Set[String] = out(default.`@id`, default.`@ids`).collect { case url: String => url }.toSet

  /**
    * @return set of all available edge-labels (keys)
    */
  def keys: Set[Property]

  /**
    * @return list of labels assigned to the resource
    */
  def `@type`: List[ClassType[Any]] = labels

  /**
    * alias for `@type`
    * @return
    */
  def labels: List[ClassType[Any]]

  protected def sameResource(resource: Resource[_]): Boolean = resource.id == id

//  override def equals(o: scala.Any): Boolean = o match {
//    case resource: graph._Resource[_] =>
//      sameResource(resource) //this does not match for an inner trait (scala-bug) but this is mitigated by the matching in Node, Edge and Value
//    case _ => false
//  }

  /**
    * The hashcode is composed by the id-hash and the graph-iri-hash
    */
  override lazy val hashCode: Int = id.hashCode() + graph.hashCode

  /** Compares resources by their data-value
    *
    * @param o
    * @return
    */
  def equalValues(o: scala.Any): Boolean

  def ===(o: scala.Any): Boolean = equalValues(o)

  /**
    * filters the resource if it is labeled (including inherited/parent labels) with one or more of the provided labels
    * @param label
    * @tparam L
    * @return
    */
  def hasLabel[L](label: ClassType[L]*): Option[Resource[L]] = {
    val _labels = labels
    if (_labels.exists(tpe => label.contains(tpe)) || {
          label.exists(label => _labels.exists(_.`extends`(label)))
        }) Some(this.asInstanceOf[Resource[L]])
    else None
  }

  implicit private def iriToPropertyKey(iri: String): Property =
    graph.ns.properties.cached(iri).getOrElse(Property(iri))
  def out(key: String, keys: String*): List[Any] =
    out((key: Property) :: keys.toList.map(key => key: Property): _*)
  def out(f: (Property.default.type => Property), ff: (Property.default.type => Property)*): List[Any] =
    out((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters out-going resources by the provided [[Property]]
    *
    * @param key the [[Edge]]'s labels
    * @return List of unboxed values
    */
  def out(key: Property*): List[Any]
  def outMap(key: String, keys: String*): Map[Property, List[Any]] =
    outMap((key: Property) :: keys.toList.map(key => key: Property): _*)
  def outMap(f: (Property.default.type => Property),
             ff: (Property.default.type => Property)*): Map[Property, List[Any]] =
    outMap((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters and groups out-going resources by the provided property-keys
    *
    * @param key the [[Edge]]'s labels to filter and group by
    * @return Map[Property, List[Any]]
    */
  def outMap(key: Property*): Map[Property, List[Any]]
  def outE(key: String, keys: String*): List[Edge[T, Any]] =
    outE((key: Property) :: keys.toList.map(key => key: Property): _*)
  def outE(f: (Property.default.type => Property), ff: (Property.default.type => Property)*): List[Edge[T, Any]] =
    outE((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters out-going resources by the provided [[Property]]
    *
    * @param key the [[Edge]]'s labels
    * @return List[Edge[T, Any]]
    */
  def outE(key: Property*): List[Edge[T, Any]]
  def outEMap(key: String, keys: String*): Map[Property, List[Edge[T, Any]]] =
    outEMap((key: Property) :: keys.toList.map(key => key: Property): _*)
  def outEMap(f: (Property.default.type => Property),
              ff: (Property.default.type => Property)*): Map[Property, List[Edge[T, Any]]] =
    outEMap((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters and groups out-going edges by the provided labels
    *
    * @param key the [[Edge]]'s labels
    * @return Map[Property, List[Edge[T, Any]]]
    */
  def outEMap(key: Property*): Map[Property, List[Edge[T, Any]]]
  def out[V](key: TypedProperty[V], keys: TypedProperty[V]*): List[V] =
    outE(key.key).flatMap(_.to.hasLabel(key.range).map(_.value)) ++ keys.flatMap(key =>
      outE(key.key).flatMap(_.to.hasLabel(key.range).map(_.value)))
  def outE[V](key: TypedProperty[V], keys: TypedProperty[V]*): List[Edge[T, V]] =
    outE(key.key).filter(_.to.hasLabel(key.range).isDefined).asInstanceOf[List[Edge[T, V]]] ++
      keys
        .flatMap(key => outE(key.key).filter(_.to.hasLabel(key.range).isDefined))
        .toList
        .asInstanceOf[List[Edge[T, V]]]
  def in(key: String, keys: String*): List[Any] =
    in((key: Property) :: keys.toList.map(key => key: Property): _*)
  def in(f: (Property.default.type => Property), ff: (Property.default.type => Property)*): List[Any] =
    in((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters in-coming resources by the provided [[Property]]
    *
    * @param key the [[Edge]]'s labels
    * @return List of unboxed values
    */
  def in(key: Property*): List[Any]

  def inMap(key: String, keys: String*): Map[Property, List[Any]] =
    inMap((key: Property) :: keys.toList.map(key => key: Property): _*)
  def inMap(f: (Property.default.type => Property),
            ff: (Property.default.type => Property)*): Map[Property, List[Any]] =
    inMap((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters and groups in-coming resources by the provided property-keys
    *
    * @param key the [[Edge]]'s labels to filter and group by
    * @return Map[Property, List[Any]]
    */
  def inMap(key: Property*): Map[Property, List[Any]]
  def inE(key: String, keys: String*): List[Edge[Any, T]] =
    inE((key: Property) :: keys.toList.map(key => key: Property): _*)
  def inE(f: (Property.default.type => Property), ff: (Property.default.type => Property)*): List[Edge[Any, T]] =
    inE((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters in-coming resources by the provided [[Property]]
    *
    * @param key the [[Edge]]'s labels
    * @return List[Edge[Any, T]]
    */
  def inE(key: Property*): List[Edge[Any, T]]
  def inEMap(key: String, keys: String*): Map[Property, List[Edge[Any, T]]] =
    inEMap((key: Property) :: keys.toList.map(key => key: Property): _*)
  def inEMap(f: (Property.default.type => Property),
             ff: (Property.default.type => Property)*): Map[Property, List[Edge[Any, T]]] =
    inEMap((f :: ff.toList).map(_.apply(Property.default)): _*)

  /** Filters and groups in-coming edges by the provided labels
    *
    * @param key the [[lspace.structure.Edge]]'s labels
    * @return Map[Property, List[Edge[Any, T]]]
    */
  def inEMap(key: Property*): Map[Property, List[Edge[Any, T]]]

  def ---(key: String): PartialOutEdge[T] =
    ---(
      graph.ns.properties
        .cached(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ))

  /** Creates a partial edge
    *
    * @param key the [[Edge]]'s label
    * @return an labeled-edge-builder [[PartialOutEdge]]
    */
  def ---(key: Property): PartialOutEdge[T] = PartialOutEdge(this, key)
  def ---(f: Property.default.type => Property): PartialOutEdge[T] =
    PartialOutEdge(this, f(Property.default))

  /**
    * Edge with Cardinality single
    * @param key
    * @return
    */
  def -|-(key: Property): PartialOutEdge[T] = ??? //TODO: drop existing, add new

  import shapeless.<:!<
  def addOut[V, V0, VT0 <: ClassType[Any]](key: String, value: V)(
      implicit ev1: V <:!< ClassType[Any],
      dt: ClassTypeable.Aux[V, V0, VT0]): Task[Edge[T, V0]] =
    addOut[V, V0, VT0](
      graph.ns.properties
        .cached(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addOut[V <: ClassType[Any]](key: String, value: V): Task[Edge[T, Node]] =
    addOut(
      graph.ns.properties
        .cached(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addOut[V, V0, VT0 <: ClassType[Any]](key: Property, value: V)(
      implicit ev1: V <:!< ClassType[Any],
      ct: ClassTypeable.Aux[V, V0, VT0]): Task[Edge[T, V0]] = {
    for {
      toResource <- value match {
        case resource: Resource[_] => graph.resources.upsert(resource)
        case _ =>
          graph.values.upsert(value, DataType.detect(value))
      }
      edge <- graph.edges.create(this, key, toResource.asInstanceOf[Resource[V0]])
    } yield edge
  }

  //TODO: cleanup types
  def addOut[V, R[Z] <: ClassType[Z]](key: Property, dt: R[V], value: V)(
      implicit ev1: V <:!< ClassType[Any]): Task[Edge[T, V]] = {
    for {
      toResource <- value match {
        case resource: Resource[_] => Task.now(resource.asInstanceOf[Resource[V]])
        case _ =>
          dt match {
            case dt: DataType[V] if dt.iri.nonEmpty =>
              graph.values.upsert(value, dt)
            case ct: ClassType[V] =>
              graph.values.upsert(value, DataType.detect(value))
          }
      }
      edge <- graph.edges.create(this, key, toResource)
    } yield edge
  }

  def addOut[V <: ClassType[Any]](key: Property, value: V): Task[Edge[T, Node]] = {
//    val toResource = graph.ns.classtypes.store(value)
//    val toResource = graph.ns.classtypes.store(value)
    for {
      resource <- graph.nodes.upsert(value.iri)
      edge     <- graph.edges.create(this, key, resource)
    } yield edge
  }

  //TODO: value can be a subtype of V, Task fails of subtype is not supported
  def addOut[V](key: TypedProperty[V], value: V): Task[Edge[T, V]] = {
    for {
      toResource <- value match {
        case resource: Resource[_] => graph.resources.upsert(resource) //.map(_.asInstanceOf[Resource[V]])
        case _                     => graph.values.upsert(value, key.range.asInstanceOf[DataType[V]])
      }
      edge <- graph.edges.create(this, key.key, toResource)
    } yield edge.asInstanceOf[Edge[T, V]]
  }

  def <--(key: String): PartialInEdge[T] =
    <--(
      graph.ns.properties
        .cached(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ))
  def <--(key: Property): PartialInEdge[T] = PartialInEdge(this, key)
  def addIn[V, V0, VT0 <: ClassType[_]](key: String, value: V)(implicit ev1: V <:!< ClassType[_],
                                                               dt: ClassTypeable.Aux[V, V0, VT0]): Task[Edge[V0, T]] =
    addIn[V, V0, VT0](
      graph.ns.properties
        .cached(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addIn[V <: ClassType[_]](key: String, value: V): Task[Edge[Node, T]] =
    addIn(
      graph.ns.properties
        .cached(key)
        .getOrElse(Property(key) /*throw new Exception("try to download unknown property")*/ ),
      value
    )
  def addIn[V, V0, VT0 <: ClassType[_]](key: Property, value: V)(
      implicit ev1: V <:!< ClassType[_],
      ct: ClassTypeable.Aux[V, V0, VT0]): Task[Edge[V0, T]] = {
    for {
      toResource <- value match {
        case resource: Resource[_] => graph.resources.upsert(resource)
        case _ =>
          graph.values.upsert(value)(ct)
      }
      edge <- graph.edges.create(toResource.asInstanceOf[Resource[V0]], key, this)
    } yield edge
  }
  def addIn[V, R[Z] <: ClassType[Z]](key: Property, dt: R[V], value: V)(
      implicit ev1: V <:!< ClassType[_]): Task[Edge[V, T]] = {
    for {
      toResource <- value match {
        case resource: Resource[_] => graph.resources.upsert(resource)
        case _ =>
          dt match {
            case dt: DataType[V] if dt.iri.nonEmpty =>
              graph.values.upsert(value, dt)
            case ct: ClassType[V] =>
              graph.values.upsert(value, DataType.detect(value))
          }
      }
      edge <- graph.edges.create(toResource, key, this)
    } yield edge.asInstanceOf[Edge[V, T]]
  }

  def addIn[V <: ClassType[_]](key: Property, value: V): Task[Edge[Node, T]] = {
//    val fromResource = graph.ns.classtypes.store(value)
//    val fromResource = graph.ns.classtypes.store(value)
    for {
      resource <- graph.nodes.upsert(value.iri)
      edge     <- graph.edges.create(resource, key, this)
    } yield edge
  }

  def addBoth[V, R[T] <: Resource[T]](key: Property, value: R[V]): Task[(Edge[T, V], Edge[V, T])] = {
    val fromCT = this match {
      case node: Node       => DataType.default.`@nodeURL`
      case edge: Edge[_, _] => DataType.default.`@edgeURL`
      case value: Value[_]  => value.label
    }
    val valueCT = value match {
      case node: Node       => DataType.default.`@nodeURL`
      case edge: Edge[_, _] => DataType.default.`@edgeURL`
      case value: Value[_]  => value.label
    }

    for {
      from <- addOut(key, valueCT.asInstanceOf[ClassType[R[V]]], value)
      to <- value
        .addOut(key, fromCT.asInstanceOf[ClassType[Resource[T]]], this.asInstanceOf[Resource[T]])
    } yield (from.asInstanceOf[Edge[T, V]], to.asInstanceOf[Edge[V, T]])
  }

  def removeIn[V >: T](edge: Edge[_, V]): Task[Unit]
  def removeOut[V >: T](edge: Edge[V, _]): Task[Unit]
  def removeIn(key: Property): Task[Unit]
  def removeOut(key: Property): Task[Unit]

  def remove(): Task[Unit]

  def prettyPrint: String
}
