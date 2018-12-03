package lspace.librarian.datatype

import lspace.NS
import lspace.NS.types
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._

import scala.collection.immutable.ListSet

object MapType {

  object keys {
    //    val keyRange = Property("@keyRange")(label = Map("en" -> "@keyRange"))
    //    keyRange --- Property.default.label --> "@keyRange" --- MemGraphDefault.language --> "en"
    //    val keyRangeOntology: TypedPropertyKey[Node] = keyRange.addRange(keyRange.graph.ontology)
    //    val keyRangeProperty: TypedPropertyKey[Node] = keyRange.addRange(keyRange.graph.property)
    //    val keyRangeDataType: TypedPropertyKey[Node] = keyRange.addRange(keyRange.graph.datatype)

    private val keyRangeNode = MemGraphDefault.ns.nodes.upsert("@keyRange")
    keyRangeNode.addLabel(Property.ontology)
    keyRangeNode --- Property.default.`@label` --> "@keyRange" --- Property.default.`@language` --> "en"
    keyRangeNode --- Property.default.`@container` --> types.`@list`
    keyRangeNode --- Property.default.`@range` --> types.`@class`
    val keyRange = Property(keyRangeNode)
  }
  //  def apply[K](keyType: ClassType[K])(implicit graph: Graph)= {
  //    val iri = s"${ldcontext.types.map}:[${keyType.iri}]"
  //    new MapType[K, Any](keyType, valueType, graph.getDataType(iri).getOrElse(graph.nodes.upsert(iri)))
  //  }
  //  def apply[K, V](keyRange: List[ClassType[K]], valueRange: List[ClassType[V]])(implicit graph: Graph) = {
  //    val iri = s"${ldcontext.types.map}:[${keyRange.map(_.iri).toList.sorted}],[${valueRange.map(_.iri).toList.sorted}]]"
  //    graph.getDataType[Map[K, V]](iri).getOrElse(new MapType(keyRange, valueRange, graph.getDataType(iri).getOrElse {
  //      val node = graph.newNode(graph.datatype)
  //      node.addOut(graph.id, iri)
  //      node.addOuts(keys.keyRange, keyRange.map(graph.nodeURLType -> _))
  //      node.addOuts(CollectionType.keys.valueRange, valueRange.map(graph.nodeURLType -> _))
  //      node
  //    }))
  //  }

  def wrap(node: Node): MapType[Any, Any] = {
    MapType(
      node.out(keys.keyRange).collect { case node: Node => node }.map(node.graph.ns.getClassType),
      node
        .out(CollectionType.keys.valueRange)
        .collect { case node: Node => node }
        .map(node.graph.ns.getClassType)
    )
  }

  def apply[KT <: ClassType[_],
            KTOut,
            CKTOut <: ClassType[KTOut],
            VT <: ClassType[_],
            VTOut,
            CVTOut <: ClassType[VTOut]](keyRange: List[KT], valueRange: List[VT])(
      implicit kclsTpbl: ClassTypeable.Aux[KT, KTOut, CKTOut],
      vclsTpbl: ClassTypeable.Aux[VT, VTOut, CVTOut]): MapType[KTOut, VTOut] =
    new MapType(keyRange.asInstanceOf[List[ClassType[KTOut]]], valueRange.asInstanceOf[List[ClassType[VTOut]]])
      .asInstanceOf[MapType[KTOut, VTOut]]

  implicit def defaultCls[
      K,
      KT[+Z] <: ClassType[Z],
      V,
      VT[+Z] <: ClassType[Z],
      KOut,
      KTOut[+Z] <: ClassType[Z],
      VOut,
      VTOut[+Z] <: ClassType[Z]
  ](implicit clsTpblK: ClassTypeable.Aux[KT[K], KOut, KTOut[KOut]],
    clsTpblV: ClassTypeable.Aux[VT[V], VOut, VTOut[VOut]])
    : ClassTypeable.Aux[MapType[K, V], Map[KOut, VOut], MapType[KOut, VOut]] =
    new ClassTypeable[MapType[K, V]] {
      type C  = Map[KOut, VOut]
      type CT = MapType[KOut, VOut]
      def ct: CT = new MapType(List(clsTpblK.ct), List(clsTpblV.ct))
    }
}

class MapType[K, V](val keyRange: List[ClassType[K]], val valueRange: List[ClassType[V]])
    extends CollectionType[Map[K, V]] {

  val iri =
    s"${NS.types.`@map`}/${keyRange.map(_.iri).sorted.mkString("+")}/${valueRange.map(_.iri).sorted.mkString("+")}"

  override val _properties: () => List[Property] = () => List(MapType.keys.keyRange, CollectionType.keys.valueRange)

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(CollectionType.default[Map[K, V]])
}
