package lspace.librarian.datatype

import lspace.NS
import lspace.NS.types
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure._

object TupleType extends DataTypeDef[TupleType[Any]] {

  lazy val datatype = new TupleType[Any] {
    val iri: String                                             = NS.types.`@tuple`
    override val label: Map[String, String]                     = Map("en" -> NS.types.`@tuple`)
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(StructuredType.datatype)
  }

  object keys extends StructuredType.Properties { //TODO: change to PropertyDef
    object _1stRange
        extends PropertyDef(
          "@1stRange",
          label = "@1stRange",
          comment = "@1stRange",
          `@extends` = () => Property.default.`@range` :: Nil,
          `@range` = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
        )
    lazy val _1stRangeClassType: TypedProperty[List[Node]] = _1stRange + ListType(
      Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
    object _2ndRange
        extends PropertyDef(
          "@2ndRange",
          label = "@2ndRange",
          comment = "@2ndRange",
          `@extends` = () => Property.default.`@range` :: Nil,
          `@range` = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
        )
    lazy val _2ndRangeClassType: TypedProperty[List[Node]] = _2ndRange + ListType(
      Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
    object _3rdRange
        extends PropertyDef(
          "@3rdRange",
          label = "@3rdRange",
          comment = "@3rdRange",
          `@extends` = () => Property.default.`@range` :: Nil,
          `@range` = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
        )
    lazy val _3rdRangeClassType: TypedProperty[List[Node]] = _3rdRange + ListType(
      Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
    object _4rdRange
        extends PropertyDef(
          "@4rdRange",
          label = "@4rdRange",
          comment = "@4rdRange",
          `@extends` = () => Property.default.`@range` :: Nil,
          `@range` = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
        )
    lazy val _4rdRangeClassType: TypedProperty[List[Node]] = _4rdRange + ListType(
      Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
  }
  override lazy val properties
    : List[Property] = keys._1stRange.property :: keys._2ndRange.property :: keys._3rdRange.property :: keys._4rdRange.property :: StructuredType.properties
  trait Properties extends StructuredType.Properties {}

//  def apply(node: Node): TupleType[_] = {
//    node.iri match {
//      case iri if iri.startsWith(types.`@tuple` + 2) =>
//        Tuple2Type(
//          node
//            .out(keys._1stRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten,
//          node
//            .out(keys._2ndRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten
//        )
//      case iri if iri.startsWith(types.`@tuple` + 3) =>
//        Tuple3Type(
//          node
//            .out(keys._1stRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten,
//          node
//            .out(keys._2ndRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten,
//          node
//            .out(keys._3rdRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten
//        )
//      case iri if iri.startsWith(types.`@tuple` + 4) =>
//        Tuple4Type(
//          node
//            .out(keys._1stRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten,
//          node
//            .out(keys._2ndRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten,
//          node
//            .out(keys._3rdRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten,
//          node
//            .out(keys._4rdRange)
//            .collect { case nodes: List[Node] => nodes.map(node.graph.ns.classtypes.get) }
//            .flatten
//        )
//    }
//  }

//  implicit def defaultTuple2[A: ClassType, B: ClassType](implicit
//                                                         cta: ClassType[A],
//                                                         ctb: ClassType[B]): ClassTypeable.Aux[Tuple2Type[(A, B), ] =
//    Tuple2Type(List(cta), List(ctb))
//
//  implicit def defaultTuple3[A: ClassType, B: ClassType, C: ClassType](implicit
//                                                                       cta: ClassType[A],
//                                                                       ctb: ClassType[B],
//                                                                       ctc: ClassType[C]): TupleType[(A, B, C)] =
//    Tuple3Type(List(cta), List(ctb), List(ctc))
//
//  implicit def defaultTuple4[A: ClassType, B: ClassType, C: ClassType, D: ClassType](
//      implicit
//      cta: ClassType[A],
//      ctb: ClassType[B],
//      ctc: ClassType[C],
//      ctd: ClassType[D]): TupleType[(A, B, C, D)] =
//    Tuple4Type(List(cta), List(ctb), List(ctc), List(ctd))
}

trait TupleType[+T] extends StructuredType[T]

object Tuple2Type extends DataTypeDef[Tuple2Type[Any, Any]] {

  lazy val datatype = new Tuple2Type[Any, Any](Nil, Nil) {
    override lazy val iri: String                               = NS.types.`@tuple` + "2"
    override val label: Map[String, String]                     = Map("en" -> s"${NS.types.`@tuple`}2")
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(TupleType.datatype)
  }

  object keys extends TupleType.Properties
  override lazy val properties: List[Property] = TupleType.properties
  trait Properties extends TupleType.Properties

  def apply[A, AT[+Z] <: ClassType[Z], B, BT[+Z] <: ClassType[Z]](_1stRange: List[AT[A]], _2ndRange: List[BT[B]]) =
    new Tuple2Type(_1stRange, _2ndRange) //TODO: ClassTypeable

  implicit def defaultCls[A, Aout, ATout <: ClassType[_], B, Bout, BTout <: ClassType[_]](
      implicit clsTpblA: ClassTypeable.Aux[A, Aout, ATout],
      clsTpblB: ClassTypeable.Aux[B, Bout, BTout])
    : ClassTypeable.Aux[Tuple2Type[A, B], (Aout, Bout), Tuple2Type[Aout, Bout]] =
    new ClassTypeable[Tuple2Type[A, B]] {
      type C  = (Aout, Bout)
      type CT = Tuple2Type[Aout, Bout]
      def ct: CT =
        new Tuple2Type[Aout, Bout](List(clsTpblA.ct).asInstanceOf[List[ClassType[Aout]]],
                                   List(clsTpblB.ct).asInstanceOf[List[ClassType[Bout]]])
    }
}
class Tuple2Type[+A, +B](val _1stRange: List[ClassType[A]], val _2ndRange: List[ClassType[B]])
    extends TupleType[(A, B)] {
  lazy val iri =
//    if (_1stRange.filter(_.iri.nonEmpty).isEmpty && _2ndRange.filter(_.iri.nonEmpty).isEmpty) NS.types.`@tuple` + "2"
//    else
    s"${types.`@tuple`}2(${_1stRange.map(_.iri).filter(_.nonEmpty).mkString("+")})(${_2ndRange.map(_.iri).filter(_.nonEmpty).mkString("+")})"

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(Tuple2Type.datatype)
}

object Tuple3Type extends DataTypeDef[Tuple3Type[Any, Any, Any]] {

  lazy val datatype = new Tuple3Type[Any, Any, Any](Nil, Nil, Nil) {
    override lazy val iri: String                               = s"${NS.types.`@tuple`}3"
    override val label: Map[String, String]                     = Map("en" -> s"${NS.types.`@tuple`}3")
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(TupleType.datatype)
  }

  object keys extends TupleType.Properties
  override lazy val properties: List[Property] = TupleType.properties
  trait Properties extends TupleType.Properties

  def apply[A, AT[+Z] <: ClassType[Z], B, BT[+Z] <: ClassType[Z], C, CT[+Z] <: ClassType[Z]](_1stRange: List[AT[A]],
                                                                                             _2ndRange: List[BT[B]],
                                                                                             _3ndRange: List[CT[C]]) =
    new Tuple3Type[A, B, C](_1stRange, _2ndRange, _3ndRange)

  implicit def defaultCls[A,
                          AT[+Z] <: ClassType[Z],
                          Aout,
                          ATout[+Z] <: ClassType[Z],
                          B,
                          BT[+Z] <: ClassType[Z],
                          Bout,
                          BTout[+Z] <: ClassType[Z],
                          C,
                          CT[+Z] <: ClassType[Z],
                          Cout,
                          CTout[+Z] <: ClassType[Z]](implicit clsTpblA: ClassTypeable.Aux[AT[A], Aout, ATout[Aout]],
                                                     clsTpblB: ClassTypeable.Aux[BT[B], Bout, BTout[Bout]],
                                                     clsTpblC: ClassTypeable.Aux[CT[C], Cout, CTout[Cout]])
    : ClassTypeable.Aux[Tuple3Type[A, B, C], (Aout, Bout, Cout), Tuple3Type[Aout, Bout, Cout]] =
    new ClassTypeable[Tuple3Type[A, B, C]] {
      type C  = (Aout, Bout, Cout)
      type CT = Tuple3Type[Aout, Bout, Cout]
      def ct: CT = Tuple3Type(List(clsTpblA.ct), List(clsTpblB.ct), List(clsTpblC.ct))
    }
}
class Tuple3Type[A, B, C](val _1stRange: List[ClassType[A]],
                          val _2ndRange: List[ClassType[B]],
                          val _3rdRange: List[ClassType[C]])
    extends TupleType[(A, B, C)] {

  lazy val iri =
//    if (_1stRange.filter(_.iri.nonEmpty).isEmpty && _2ndRange.filter(_.iri.nonEmpty).isEmpty && _3rdRange
//          .filter(_.iri.nonEmpty)
//          .isEmpty) NS.types.`@tuple` + "3"
//    else
    s"${types.`@tuple`}3(${_1stRange.map(_.iri).filter(_.nonEmpty).mkString("+")})(${_2ndRange
      .map(_.iri)
      .filter(_.nonEmpty)
      .mkString("+")})(${_3rdRange.map(_.iri).filter(_.nonEmpty).mkString("+")})"

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(Tuple3Type.datatype)
}

object Tuple4Type extends DataTypeDef[Tuple4Type[Any, Any, Any, Any]] {

  lazy val datatype = new Tuple4Type[Any, Any, Any, Any](Nil, Nil, Nil, Nil) {
    override lazy val iri: String                               = s"${NS.types.`@tuple`}4"
    override val label: Map[String, String]                     = Map("en" -> s"${NS.types.`@tuple`}4")
    override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(TupleType.datatype)
  }

  object keys extends TupleType.Properties
  override lazy val properties: List[Property] = TupleType.properties
  trait Properties extends TupleType.Properties

  def apply[A, AT[+Z] <: ClassType[Z], B, BT[+Z] <: ClassType[Z], C, CT[+Z] <: ClassType[Z], D, DT[+Z] <: ClassType[Z]](
      _1stRange: List[AT[A]],
      _2ndRange: List[BT[B]],
      _3ndRange: List[CT[C]],
      _4ndRange: List[DT[D]]) =
    new Tuple4Type[A, B, C, D](_1stRange, _2ndRange, _3ndRange, _4ndRange)

  implicit def defaultCls[A,
                          AT[+Z] <: ClassType[Z],
                          Aout,
                          ATout[+Z] <: ClassType[Z],
                          B,
                          BT[+Z] <: ClassType[Z],
                          Bout,
                          BTout[+Z] <: ClassType[Z],
                          C,
                          CT[+Z] <: ClassType[Z],
                          Cout,
                          CTout[+Z] <: ClassType[Z],
                          D,
                          DT[+Z] <: ClassType[Z],
                          Dout,
                          DTout[+Z] <: ClassType[Z]](implicit clsTpblA: ClassTypeable.Aux[AT[A], Aout, ATout[Aout]],
                                                     clsTpblB: ClassTypeable.Aux[BT[B], Bout, BTout[Bout]],
                                                     clsTpblC: ClassTypeable.Aux[CT[C], Cout, CTout[Cout]],
                                                     clsTpblD: ClassTypeable.Aux[DT[D], Dout, DTout[Dout]])
    : ClassTypeable.Aux[Tuple4Type[A, B, C, D], (Aout, Bout, Cout, Dout), Tuple4Type[Aout, Bout, Cout, Dout]] =
    new ClassTypeable[Tuple4Type[A, B, C, D]] {
      type C  = (Aout, Bout, Cout, Dout)
      type CT = Tuple4Type[Aout, Bout, Cout, Dout]
      def ct: CT = Tuple4Type(List(clsTpblA.ct), List(clsTpblB.ct), List(clsTpblC.ct), List(clsTpblD.ct))
    }
}
class Tuple4Type[A, B, C, D](val _1stRange: List[ClassType[A]],
                             val _2ndRange: List[ClassType[B]],
                             val _3rdRange: List[ClassType[C]],
                             val _4rdRange: List[ClassType[D]])
    extends TupleType[(A, B, C, D)] {

  lazy val iri =
//    if (_1stRange.filter(_.iri.nonEmpty).isEmpty && _2ndRange.filter(_.iri.nonEmpty).isEmpty && _3rdRange
//          .filter(_.iri.nonEmpty)
//          .isEmpty && _4rdRange.filter(_.iri.nonEmpty).isEmpty) NS.types.`@tuple` + "4"
//    else
    s"${types.`@tuple`}4(${_1stRange.map(_.iri).filter(_.nonEmpty).mkString("+")})(${_2ndRange.map(_.iri).filter(_.nonEmpty).mkString("+")})" +
      s"(${_3rdRange.map(_.iri).filter(_.nonEmpty).mkString("+")})(${_4rdRange.map(_.iri).filter(_.nonEmpty).mkString("+")})"

  override val _extendedClasses: () => List[_ <: DataType[_]] = () => List(Tuple4Type.datatype)
}
