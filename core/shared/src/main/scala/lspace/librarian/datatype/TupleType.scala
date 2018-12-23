package lspace.librarian.datatype

import lspace.NS.types
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure._

object TupleType {
  lazy val ontology = Ontology(types.`@tuple`, extendedClasses = List(StructuredValue.ontology))

  object keys {
    private val _1stRangeNode = MemGraphDefault.ns.nodes.upsert("@1stRange")
    _1stRangeNode.addLabel(Property.ontology)
    _1stRangeNode --- Property.default.`@label` --> "@1stRange" --- Property.default.`@language` --> "en"
    _1stRangeNode --- Property.default.`@container` --> types.`@list`
    _1stRangeNode --- Property.default.`@range` --> types.`@class`
    val _1stRange = Property(_1stRangeNode)

    private val _2ndRangeNode = MemGraphDefault.ns.nodes.upsert("@2ndRange")
    _2ndRangeNode.addLabel(Property.ontology)
    _2ndRangeNode --- Property.default.`@label` --> "@1stRange" --- Property.default.`@language` --> "en"
    _2ndRangeNode --- Property.default.`@container` --> types.`@list`
    _2ndRangeNode --- Property.default.`@range` --> types.`@class`
    val _2ndRange = Property(_2ndRangeNode)

    private val _3rdRangeNode = MemGraphDefault.ns.nodes.upsert("@3rdRange")
    _3rdRangeNode.addLabel(Property.ontology)
    _3rdRangeNode --- Property.default.`@label` --> "@1stRange" --- Property.default.`@language` --> "en"
    _3rdRangeNode --- Property.default.`@container` --> types.`@list`
    _3rdRangeNode --- Property.default.`@range` --> types.`@class`
    val _3rdRange = Property(_3rdRangeNode)

    private val _4rdRangeNode = MemGraphDefault.ns.nodes.upsert("@4rdRange")
    _4rdRangeNode.addLabel(Property.ontology)
    _4rdRangeNode --- Property.default.`@label` --> "@1stRange" --- Property.default.`@language` --> "en"
    _4rdRangeNode --- Property.default.`@container` --> types.`@list`
    _4rdRangeNode --- Property.default.`@range` --> types.`@class`
    val _4rdRange = Property(_4rdRangeNode)
  }

  def apply(node: Node): TupleType[_] = {
    node.iri match {
      case iri if iri.startsWith(types.`@tuple` + 2) =>
        Tuple2Type(
          node
            .out(keys._1stRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType),
          node
            .out(keys._2ndRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType)
        )
      case iri if iri.startsWith(types.`@tuple` + 3) =>
        Tuple3Type(
          node
            .out(keys._1stRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType),
          node
            .out(keys._2ndRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType),
          node
            .out(keys._3rdRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType)
        )
      case iri if iri.startsWith(types.`@tuple` + 4) =>
        Tuple4Type(
          node
            .out(keys._1stRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType),
          node
            .out(keys._2ndRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType),
          node
            .out(keys._3rdRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType),
          node
            .out(keys._4rdRange)
            .collect { case node: Node => node }
            .map(node.graph.ns.getClassType)
        )
    }
  }

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

trait TupleType[+T] extends StructuredValue[T] {}

object Tuple2Type {

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

  val iri =
    s"${types.`@tuple`}2/${_1stRange.map(_.iri).sorted.mkString("+")}/${_2ndRange.map(_.iri).sorted.mkString("+")}"
}

object Tuple3Type {

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
                          val _3ndRange: List[ClassType[C]])
    extends TupleType[(A, B, C)] {

  val iri =
    s"${types.`@tuple`}3/${_1stRange.map(_.iri).sorted.mkString("+")}/${_2ndRange.map(_.iri).sorted.mkString("+")}/${_3ndRange.map(_.iri).sorted.mkString("+")}"
}

object Tuple4Type {

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
                             val _3ndRange: List[ClassType[C]],
                             val _4ndRange: List[ClassType[D]])
    extends TupleType[(A, B, C, D)] {

  val iri =
    s"${types.`@tuple`}4/${_1stRange.map(_.iri).sorted.mkString("+")}/${_2ndRange.map(_.iri).sorted.mkString("+")}" +
      s"/${_3ndRange.map(_.iri).sorted.mkString("+")}/${_4ndRange.map(_.iri).sorted.mkString("+")}"
}
