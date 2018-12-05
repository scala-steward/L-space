package lspace.librarian.process.traversal

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.NS
import lspace.librarian.datatype._
import lspace.librarian.process.traversal.helper.{ClassTypeable, Selector}
import lspace.librarian.process.traversal.step.Order.Orderable
import lspace.librarian.process.traversal.step.Select.Selection
import lspace.librarian.process.traversal.step._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem._
import lspace.librarian.structure._
import lspace.librarian.structure.Property.default._
import lspace.util.types.DefaultsToAny
import shapeless.{::, <:!<, =:!=, HList, HNil, LUBConstraint, Poly1, Id => _, Path => _, Select => _}
import shapeless.ops.hlist.{Collect, Mapper, Reverse, ToList, ToTraversable, Unifier}

import scala.collection.immutable.ListSet

object Traversal {

  def wrap[ET <: ClassType[_]](node: Node)(target: Graph)(et: ET): Traversal[ClassType[Any], ET, HList] = {
    implicit val graph: Graph = target

    node match {
      case node: Traversal[ClassType[Any], ET, HList] => node
      case _                                          => Traversal[ClassType[Any], ET](node)(graph, ClassType.default[Any], et)
    }
  }

  val keys = new {
    private val stepNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/Traversal/step")
    stepNode.addLabel(Property.ontology)
    stepNode --- `@label` --> "step" --- `@language` --> "en"
    stepNode --- `@comment` --> "A step in a traversal" --- `@language` --> "en"
    stepNode --- `@container` --> NS.types.`@list`
    stepNode --- `@range` --> Step.ontology
    lazy val step: Property = Property(stepNode)

    val stepStep: TypedProperty[Node] = step + Step.ontology
  }

  private val ontologyNode = MemGraphDefault.ns.nodes.create(Ontology.ontology)
  ontologyNode --- `@id` --> lspace.NS.vocab.Lspace.+("librarian/Traversal").toString
  ontologyNode --- `@label` --> "Traversal" --- `@language` --> "en"
  ontologyNode --- `@properties` --> keys.step

  lazy val ontology: Ontology = Ontology(ontologyNode)

  trait StepsHelper[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList] {
    protected[this] def _traversal: Traversal[ST, ET, Steps]
    implicit def target: Graph
    def st: ST = _traversal.st
    def et: ET = _traversal.et
  }
  trait ResourceSteps[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends StepsHelper[ST[Start], ET[End], Steps] {

    def G(graph: Graph*) =
      Traversal[ST[Start], DataType[Graph], step.G :: Steps](step.G(graph.toList) :: _traversal.steps)(
        target,
        st,
        GraphType.default)

    def N(): Traversal[ST[Start], NodeURLType[Node], step.N :: Steps] =
      Traversal[ST[Start], NodeURLType[Node], step.N :: Steps](step.N() :: _traversal.steps)(target,
                                                                                             st,
                                                                                             NodeURLType.default)
    def N(resource: Node, resources: Node*): Traversal[ST[Start], NodeURLType[Node], step.N :: Steps] =
      Traversal[ST[Start], NodeURLType[Node], step.N :: Steps](
        step.N(resource :: resources.toList) :: _traversal.steps)(target, st, NodeURLType.default)

    def E: Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], step.E :: Steps] = //: Traversal[ST[Start],ClassType[Edge[S, E]], step.E :: Steps] =
      E[Any, Any]()
    def E[S: DefaultsToAny, E: DefaultsToAny](resource: Edge[S, E]*) =
      Traversal[ST[Start], EdgeURLType[Edge[S, E]], step.E :: Steps](
        step.E(resource.toList.asInstanceOf[List[Edge[Any, Any]]]) :: _traversal.steps)(
        target,
        st,
        EdgeURLType.edgeUrlType[Edge[S, E]])

    def V(): Traversal[ST[Start], DataType[Any], V :: Steps] =
      Traversal[ST[Start], DataType[Any], step.V :: Steps](step.V() :: _traversal.steps)(target,
                                                                                         st,
                                                                                         DataType.default.default)
    def V[T, OutC, Out <: ClassType[OutC]](value: T, values: T*)(implicit cls: ClassTypeable.Aux[T, OutC, Out]) =
      Traversal[ST[Start], Out, step.V :: Steps](step.V(value :: values.toList) :: _traversal.steps)(target, st, cls.ct)

    //    def R[T <: Resource[T]: DefaultsToAny]: Traversal[Start, T, step.R :: Steps] = R()
    //    def R[T <: Resource[T]: DefaultsToAny](value: T*): Traversal[Start, T, step.R :: Steps] =
    //      Traversal[Start, T, step.R :: Steps](step.R(value.toList.asInstanceOf[List[Resource[Any]]]) :: _traversal.steps)
  }

  trait FilterStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit private def labelToProperty[L: HasStep.PropertyLabel](label: L): Property =
      label match {
        case label: Property => label
        case label: String =>
          target.ns
            .getProperty(label)
            .getOrElse(Property(label)) //throw new Exception("unknown key"))
      }
    //    def has[L: (String |∨| Property)#λ](label: L): Traversal[Start, End, Has :: Steps] = {
    def has[L: HasStep.PropertyLabel](label: L) = {
      Traversal[ST[Start], ET[End], Has :: Steps](Has(label) :: _traversal.steps)(target, st, et)
    }
    def has[L: HasStep.PropertyLabel, T <: HList](label: L, values: T)(implicit d: LUBConstraint[T, P[_]],
                                                                       ev: ToList[T, P[_]]) = {
      val step = Has(label, values.toList)
      Traversal[ST[Start], ET[End], Has :: Steps](step :: _traversal.steps)(target, st, et)
    }

    def has[L: HasStep.PropertyLabel, T](label: L, value: P[T], values: P[T]*) = {
      val step = Has(label, value :: values.toList)
      Traversal[ST[Start], ET[End], Has :: Steps](step :: _traversal.steps)(target, st, et)
    }
    def hasNot[L: HasStep.PropertyLabel](label: L) = {
      Traversal[ST[Start], ET[End], HasNot :: Steps](HasNot(label) :: _traversal.steps)(target, st, et)
    }
    def hasNot[L: HasStep.PropertyLabel, T](label: L, value: P[T], values: P[T]*) = {
      val step = HasNot(label, value :: values.toList)
      Traversal[ST[Start], ET[End], HasNot :: Steps](step :: _traversal.steps)(target, st, et)
    }

    def hasId(id: Long, ids: Long*) =
      Traversal[ST[Start], ET[End], HasId :: Steps](HasId(id :: ids.toList toSet) :: _traversal.steps)(target, st, et)
    def hasId(ids: Set[Long]) =
      Traversal[ST[Start], ET[End], HasId :: Steps](HasId(ids) :: _traversal.steps)(target, st, et)

    /**
      * has at least one of the provided iris
      * @param uri
      * @param uris
      * @return
      */
    def hasIri(iri: String, uris: String*) =
      Traversal[ST[Start], ET[End], HasIri :: Steps](HasIri(iri :: uris.toList toSet) :: _traversal.steps)(target,
                                                                                                           st,
                                                                                                           et)
    def hasIri(iris: Set[String]) =
      Traversal[ST[Start], ET[End], HasIri :: Steps](HasIri(iris) :: _traversal.steps)(target, st, et)

    /**
      * has at least one of the provided labels
      * @param label
      * @param labels
      * @tparam T
      * @return
      */
//    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1]](label0: ET0, labels: ET0*)(
//        implicit et: ClassTypeable.Aux[ET0, End1, ET1]) = {
//      Traversal[ST[Start], ET1, HasLabel :: Steps](HasLabel(label0 :: labels.toList) :: _traversal.steps)(target,
//                                                                                                          st,
//                                                                                                          et.ct)
//    }

    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1]](label0: ET0)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]) = {
      Traversal[ST[Start], ET1, HasLabel :: Steps](HasLabel(label0 :: Nil) :: _traversal.steps)(target, st, et.ct)
    }

    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1]](label0: ET0, label1: ET0)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]) = {
      Traversal[ST[Start], ET1, HasLabel :: Steps](HasLabel(label0 :: label1 :: Nil) :: _traversal.steps)(target,
                                                                                                          st,
                                                                                                          et.ct)
    }
    def hasLabel[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[End1]](
        label0: ET0[T],
        label1: ET0[T],
        label2: ET0[T])(implicit et: ClassTypeable.Aux[ET0[T], End1, ET1]) = {
      Traversal[ST[Start], ET1, HasLabel :: Steps](HasLabel(label0 :: label1 :: label2 :: Nil) :: _traversal.steps)(
        target,
        st,
        et.ct)
    }
    def hasLabel[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[End1]](
        label0: ET0[T],
        label1: ET0[T],
        label2: ET0[T],
        label3: ET0[T])(implicit et: ClassTypeable.Aux[ET0[T], End1, ET1]) = {
      Traversal[ST[Start], ET1, HasLabel :: Steps](
        HasLabel(label0 :: label1 :: label2 :: label3 :: Nil) :: _traversal.steps)(target, st, et.ct)
    }

//    def hasLabel[T](implicit ev: T <:!< ClassType[_], label: _ <: ClassType[T]) = {
//      Traversal(HasLabel(label :: Nil) :: _traversal.steps)(target, st, label)
//    }
    def hasLabel[A](implicit cls: ClassTypeable[A]) = {
      Traversal[ST[Start], cls.CT, HasLabel :: Steps](HasLabel(cls.ct :: Nil) :: _traversal.steps)(target, st, cls.ct)
    }

//    def hasLabel[A,B](implicit clsA: ClassTypeable[A], clsB: ClassTypeable[B]) = {
//      Traversal[ST[Start], cls.CT, HasLabel :: Steps](HasLabel(cls.ct :: Nil) :: _traversal.steps)(target, st, cls.ct)
//    }

    def isNumber =
      Traversal[ST[Start], NumericType[Any], HasLabel :: Steps](HasLabel(
        DataType.default.`@int` :: DataType.default.`@double` :: DataType.default.`@long` :: Nil) :: _traversal.steps)(
        target,
        st,
        DataType.default.`@number`)

    def isTemporal =
      Traversal[ST[Start], CalendarType[Any], HasLabel :: Steps](HasLabel(
        DataType.default.`@datetime` :: DataType.default.`@datetime` :: DataType.default.`@time` :: Nil) :: _traversal.steps)(
        target,
        st,
        DataType.default.`@temporal`)

    def isQuantity =
      Traversal[ST[Start], QuantityType[Any], HasLabel :: Steps](
        HasLabel(DataType.default.`@duration` :: Nil) :: _traversal.steps)(target, st, DataType.default.`@quantity`)

    def isDuration =
      Traversal[ST[Start], DurationType, HasLabel :: Steps](
        HasLabel(DataType.default.`@duration` :: Nil) :: _traversal.steps)(target, st, DataType.default.`@duration`)

    def isGeo =
      Traversal[ST[Start], GeometricType[Any], HasLabel :: Steps](
        HasLabel(DataType.default.`@geopoint` :: Nil) :: _traversal.steps)(target, st, DataType.default.`@geo`)

    def isColor =
      Traversal[ST[Start], ColorType[Any], HasLabel :: Steps](
        HasLabel(DataType.default.`@color` :: Nil) :: _traversal.steps)(target, st, DataType.default.`@color`)

    def hasLabel(label0: String, labels0: String*) = {
      val labelKeys = (target.ns
        .getClassType(label0) ::
        labels0
        .map(label => target.ns.getClassType(label))
        .toList).flatten
      Traversal[ST[Start], ET[End], HasLabel :: Steps](HasLabel(labelKeys) :: _traversal.steps)(target, st, et)
    }

    def coin(p: Double) =
      Traversal[ST[Start], ET[End], Coin :: Steps](Coin(p) :: _traversal.steps)(target, st, et)
  }

  trait MoveMapStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends StepsHelper[ST[Start], ET[End], Steps] {

    def outMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], OutMap :: Steps] =
      outMap(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def outMap(keys: List[Property]): Traversal[ST[Start], ClassType[Any], OutMap :: Steps] =
      outMap(keys: _*)
    def outMap(key: Property*): Traversal[ST[Start], ClassType[Any], OutMap :: Steps] =
      Traversal[ST[Start], ClassType[Any], OutMap :: Steps](OutMap(key.toSet) :: _traversal.steps)(
        target,
        st,
        ClassType.default[Any])

    def outEMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Edge[End, Any]], OutEMap :: Steps] =
      outEMap(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def outEMap(keys: List[Property]): Traversal[ST[Start], ClassType[Edge[End, Any]], OutEMap :: Steps] =
      outEMap(keys: _*)
    def outEMap(key: Property*): Traversal[ST[Start], ClassType[Edge[End, Any]], OutEMap :: Steps] =
      Traversal[ST[Start], ClassType[Edge[End, Any]], OutEMap :: Steps](OutEMap(key.toSet) :: _traversal.steps)(
        target,
        st,
        EdgeURLType.edgeUrlType[Edge[End, Any]])

    def inMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], InMap :: Steps] =
      inMap(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def inMap(keys: List[Property]): Traversal[ST[Start], ClassType[Any], InMap :: Steps] =
      inMap(keys: _*)
    def inMap(key: Property*): Traversal[ST[Start], ClassType[Any], InMap :: Steps] =
      Traversal[ST[Start], ClassType[Any], InMap :: Steps](InMap(key.toSet) :: _traversal.steps)(target,
                                                                                                 st,
                                                                                                 ClassType.default[Any])

    def inEMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Edge[Any, End]], InEMap :: Steps] =
      inEMap(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def inEMap(keys: List[Property]): Traversal[ST[Start], ClassType[Edge[Any, End]], InEMap :: Steps] =
      inEMap(keys: _*)
    def inEMap(key: Property*): Traversal[ST[Start], ClassType[Edge[Any, End]], InEMap :: Steps] =
      Traversal[ST[Start], ClassType[Edge[Any, End]], InEMap :: Steps](InEMap(key.toSet) :: _traversal.steps)(
        target,
        st,
        EdgeURLType.edgeUrlType[Edge[Any, End]])
  }

  trait MoveStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends StepsHelper[ST[Start], ET[End], Steps] {

    def out(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], Out :: Steps] =
      out(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def out(keys: List[Property] = List()): Traversal[ST[Start], ClassType[Any], Out :: Steps] =
      out(keys: _*)
    def out(key: Property*): Traversal[ST[Start], ClassType[Any], Out :: Steps] =
      Traversal[ST[Start], ClassType[Any], Out :: Steps](Out(key.toSet) :: _traversal.steps)(target,
                                                                                             st,
                                                                                             ClassType.default[Any])
    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V])(
        implicit et: ClassTypeable.Aux[V, End1, ET1]): Traversal[ST[Start], ET1, HasLabel :: Out :: Steps] =
      _traversal.out(key.key).hasLabel[V](et)

    def outE(key: String, keys: String*): Traversal[ST[Start], ClassType[Edge[End, Any]], OutE :: Steps] =
      outE(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def outE(keys: List[Property]): Traversal[ST[Start], ClassType[Edge[End, Any]], OutE :: Steps] =
      outE(keys: _*)
    def outE(key: Property*): Traversal[ST[Start], ClassType[Edge[End, Any]], OutE :: Steps] =
      Traversal[ST[Start], ClassType[Edge[End, Any]], OutE :: Steps](OutE(key.toSet) :: _traversal.steps)(
        target,
        st,
        EdgeURLType.edgeUrlType[Edge[End, Any]])

    def in(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], In :: Steps] =
      in(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def in(keys: List[Property]): Traversal[ST[Start], ClassType[Any], In :: Steps] =
      in(keys: _*)
    def in(key: Property*): Traversal[ST[Start], ClassType[Any], In :: Steps] =
      Traversal[ST[Start], ClassType[Any], In :: Steps](In(key.toSet) :: _traversal.steps)(target,
                                                                                           st,
                                                                                           ClassType.default[Any])

    def inE(key: String, keys: String*): Traversal[ST[Start], ClassType[Edge[Any, End]], InE :: Steps] =
      inE(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def inE(keys: List[Property]): Traversal[ST[Start], ClassType[Edge[Any, End]], InE :: Steps] =
      inE(keys: _*)
    def inE(key: Property*): Traversal[ST[Start], ClassType[Edge[Any, End]], InE :: Steps] =
      Traversal[ST[Start], ClassType[Edge[Any, End]], InE :: Steps](InE(key.toSet) :: _traversal.steps)(
        target,
        st,
        EdgeURLType.edgeUrlType[Edge[Any, End]])

  }

  trait CommonStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends StepsHelper[ST[Start], ET[End], Steps] {
    def drop() = {
      Traversal[ST[Start], ET[End], Drop :: Steps](Drop() :: _traversal.steps)(target, st, et)
    }

    def dedup() = {
      Traversal[ST[Start], ET[End], Dedup :: Steps](Dedup() :: _traversal.steps)(target, st, et)
    }

    def as[S <: String](name: () => S): Traversal[ST[Start], ET[End], As[End, S] :: Steps] =
      Traversal[ST[Start], ET[End], As[End, S] :: Steps](As[End, S](name()) :: _traversal.steps)(target, st, et)

    def group[AZ <: ClassType[_]](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ, _ <: HList]) =
      Traversal[ST[Start], ET[End], Group[AZ] :: Steps](
        Group[AZ](by(Traversal[ET[End], ET[End]]()(target, et, et))) :: _traversal.steps)(target, st, et)
    //    def project[T, R](by: (Traversal[End, End, LastStep :: Steps] => Traversal[End, T, _, _ <: HList])*)
    //                     (traversals: List[Traversal[End, T, _, _ <: HList]] = by.toList.map(_(Traversal[End, End, LastStep]())))
    //                  (implicit f: FooTest.Aux[T, R], m: Monoid[R])
    //    : Traversal[Start, R, Project :: Steps] = {}

    private def stubList[T]: List[T] = List[T]()
    def project[A, AZ[+Z] <: ClassType[Z], B, BZ[+Z] <: ClassType[Z], ABZ <: ClassType[_]](
        by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], _ <: HList],
        by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], _ <: HList])(
        implicit
        listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: HNil, List, ABZ]
    ) /*: Traversal[ST[Start], Tuple2Type[A,B], Project :: Steps]*/ = {
      val tby1 = by1(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby2 = by2(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal(Project(List(tby1, tby2).asInstanceOf[List[Traversal[ET[End], ABZ, HList]]]) :: _traversal.steps)(
        target,
        st,
        Tuple2Type(List(tby1.et), List(tby2.et)))
    }

    def project[A, AZ[+Z] <: ClassType[Z], B, BZ[+Z] <: ClassType[Z], C, CZ[+Z] <: ClassType[Z], ABCZ <: ClassType[_]](
        by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], _ <: HList],
        by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], _ <: HList],
        by3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CZ[C], _ <: HList])(
        implicit listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: CZ[C] :: HNil, List, ABCZ]
    ) /*: Traversal[ST[Start], Tuple3Type[AZ[A], BZ[B], CZ[C]], Project :: Steps]*/ = {
      val tby1 = by1(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby2 = by2(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby3 = by3(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal(
        Project(List(tby1, tby2, tby3)
          .asInstanceOf[List[Traversal[ET[End], ABCZ, HList]]]) :: _traversal.steps)(
        target,
        st,
        Tuple3Type(List(tby1.et), List(tby2.et), List(tby3.et)))
    }

    def project[A,
                AZ[+Z] <: ClassType[Z],
                B,
                BZ[+Z] <: ClassType[Z],
                C,
                CZ[+Z] <: ClassType[Z],
                D,
                DZ[+Z] <: ClassType[Z],
                ABCDZ <: ClassType[_]](by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], _ <: HList],
                                       by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], _ <: HList],
                                       by3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CZ[C], _ <: HList],
                                       by4: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], DZ[D], _ <: HList])(
        implicit listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: CZ[C] :: DZ[D] :: HNil, List, ABCDZ]
    ) /*: Traversal[ST[Start], Tuple4Type[AZ[A], BZ[B], CZ[C], DZ[D]], Project :: Steps]*/ = {
      val tby1 = by1(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby2 = by2(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby3 = by3(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby4 = by4(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal(
        Project(List(tby1, tby2, tby3, tby4).asInstanceOf[List[Traversal[ET[End], ABCDZ, HList]]]) :: _traversal.steps)(
        target,
        st,
        Tuple4Type(List(tby1.et), List(tby2.et), List(tby3.et), List(tby4.et)))
    }

    def where(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList]) =
      Traversal[ST[Start], ET[End], Where :: Steps](
        Where(traversal(Traversal[ET[End], ET[End]]()(target, et, et))) :: _traversal.steps)(target, st, et)

    def and(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
            traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*) =
      Traversal[ST[Start], ET[End], And :: Steps](
        And(
          traversal(Traversal[ET[End], ET[End]]()(target, et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]]()(target, et, et)))) :: _traversal.steps)(target,
                                                                                                              st,
                                                                                                              et)

    def or(
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
        traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*
    ) =
      Traversal[ST[Start], ET[End], Or :: Steps](
        Or(
          traversal(Traversal[ET[End], ET[End]]()(target, et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]]()(target, et, et)))) :: _traversal.steps)(
        target,
        st,
        et) //.asInstanceOf[List[Traversal[End, Any, Step]]]).asInstanceOf[Step])

    def not(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList]) =
      Traversal[ST[Start], ET[End], Not :: Steps](
        Not(traversal(Traversal[ET[End], ET[End]]()(target, et, et))) :: _traversal.steps)(target, st, et)

    import shapeless.ops.hlist.LeftFolder
    import shapeless.ops.hlist.LeftReducer
//    def union[ET1 <: ClassType[_],
//              ET2 <: ClassType[_],
////              H <: ClassType[_],
////              ENDS <: HList,
////              UENDS <: HList,
//              ETc <: ClassType[_],
//              ETout <: ClassType[_],
//              Steps1 <: HList,
//              Steps2 <: HList](t1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET1, Steps1],
//                               t2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET2, Steps2])(
//        implicit
//        endsReducer: LeftFolder.Aux[ET2 :: HNil, List[ET1], EndFolder.type, List[ETc]],
//        ev: ToList[Traversal[ET[End], ET1, Steps1] :: Traversal[ET[End], ET2, Steps2] :: HNil,
//                   Traversal[ET[End], _ <: ClassType[_], _ <: HList]],
//        et0: ClassTypeable.Aux[ETc, ETout]): Traversal[ST[Start], ETout, Union[ET[End], ETout] :: Steps] = {
//      union(
//        t1(Traversal[ET[End], ET[End]]()(target, et, et)) :: t2(Traversal[ET[End], ET[End]]()(target, et, et)) :: HNil)(
//        endsReducer,
//        ev,
//        et0)
//    }
//
//    def union[ET1 <: ClassType[_],
//              ET2 <: ClassType[_],
//              ET3 <: ClassType[_],
//              ETc <: ClassType[_],
////              H <: ClassType[_],
////              ENDS <: HList,
////              UENDS <: HList,
//              ETout <: ClassType[_],
//              Steps1 <: HList,
//              Steps2 <: HList,
//              Steps3 <: HList](t1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET1, Steps1],
//                               t2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET2, Steps2],
//                               t3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET3, Steps3])(
//        implicit
//        endsReducer: LeftFolder.Aux[ET2 :: ET3 :: HNil, List[ET1], EndFolder.type, List[ETc]],
//        ev: ToList[
//          Traversal[ET[End], ET1, Steps1] :: Traversal[ET[End], ET2, Steps2] :: Traversal[ET[End], ET3, Steps3] :: HNil,
//          Traversal[ET[End], _ <: ClassType[_], _ <: HList]],
//        et0: ClassTypeable.Aux[ETc, ETout]): Traversal[ST[Start], ETout, Union[ET[End], ETout] :: Steps] = {
//      union(
//        t1(Traversal[ET[End], ET[End]]()(target, et, et)) :: t2(Traversal[ET[End], ET[End]]()(target, et, et)) :: t3(
//          Traversal[ET[End], ET[End]]()(target, et, et)) :: HNil)(endsReducer, ev, et0)
//    }
//
//    def union[ET1 <: ClassType[_],
//              ET2 <: ClassType[_],
//              ET3 <: ClassType[_],
//              ET4 <: ClassType[_],
//              ETc <: ClassType[_],
////              H <: ClassType[_],
////              ENDS <: HList,
////              UENDS <: HList,
//              ETout <: ClassType[_],
//              Steps1 <: HList,
//              Steps2 <: HList,
//              Steps3 <: HList,
//              Steps4 <: HList](t1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET1, Steps1],
//                               t2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET2, Steps2],
//                               t3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET3, Steps3],
//                               t4: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET4, Steps4])(
//        implicit
//        endsReducer: LeftFolder.Aux[ET2 :: ET3 :: ET4 :: HNil, List[ET1], EndFolder.type, List[ETc]],
//        ev: ToList[
//          Traversal[ET[End], ET1, Steps1] :: Traversal[ET[End], ET2, Steps2] :: Traversal[ET[End], ET3, Steps3] :: Traversal[
//            ET[End],
//            ET4,
//            Steps4] :: HNil,
//          Traversal[ET[End], _ <: ClassType[_], _ <: HList]],
//        et0: ClassTypeable.Aux[ETc, ETout]): Traversal[ST[Start], ETout, Union[ET[End], ETout] :: Steps] = {
//      union(
//        t1(Traversal[ET[End], ET[End]]()(target, et, et)) :: t2(Traversal[ET[End], ET[End]]()(target, et, et)) :: t3(
//          Traversal[ET[End], ET[End]]()(target, et, et)) :: t4(Traversal[ET[End], ET[End]]()(target, et, et)) :: HNil)(
//        endsReducer,
//        ev,
//        et0)
//    }
//
//    def union[ETc <: ClassType[_], ETout <: ClassType[_], TRAVERSALS <: HList, ENDS <: HList](traversals: TRAVERSALS)(
//        implicit
//        lub: LUBConstraint[TRAVERSALS, Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
//        mapper: Mapper.Aux[TraveralEndMapper.type, TRAVERSALS, ENDS],
////        endsUnifier: Unifier.Aux[ENDS, ETc :: UENDS],
////        endsReducer: LeftFolder.Aux[ENDS, List[H], EndFolder.type, List[ETc]],
//        endsReducer: LeftReducer.Aux[TRAVERSALS, TraversalEndsUnifier.type, ETc],
//        ev: ToList[TRAVERSALS, Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
//        end: ClassTypeable.Aux[ETc, ETout]): Traversal[ST[Start], ETout, Union[ET[End], ETout] :: Steps] = {
//      Traversal(Union(ev(traversals).map(_.asInstanceOf[Traversal[ET[End], ETout, HList]])) :: _traversal.steps)(target,
//                                                                                                                 st,
//                                                                                                                 end.ct)
//    }

    def union[ET0 <: ClassType[_],
              End1,
              ET1 <: ClassType[End1],
              Steps1 <: HList,
              Steps2 <: HList,
              Labels1 <: HList,
              Labels2 <: HList](traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps1],
                                traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps2])*)(
        implicit
        f1: Collect.Aux[Steps1, LabelSteps.type, Labels1],
        f2: Collect.Aux[Steps2, LabelSteps.type, Labels2],
        ev2: Labels1 =:= Labels2,
        et0: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Union[ET[End], ET0] :: Steps] = {
      val unionTraversal = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal[ST[Start], ET1, Union[ET[End], ET0] :: Steps](
        Union[ET[End], ET0](
          unionTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]]()(target, et, et))
              .asInstanceOf[Traversal[ET[End], ET0, HList]])) :: _traversal.steps)(target, st, et0.ct)
    }

    /**
      *
      * @param traversal to be repeated
      * @param until nonempty traversal is being repeated
      * @param max number of times is being repeated
      * @param collect result of each loop
      * @return
      */
    def repeat[ET0 <: ClassType[_]](
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList],
        until: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList] = null,
        max: Int = 0,
        collect: Boolean = false) = {
      val t = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal[ST[Start], ET0, Repeat[ET0] :: Steps](
        Repeat(
          t,
          Option(until).map(_(Traversal[ET[End], ET[End]]()(target, et, et))),
          if (max == 0) None else Some(max),
          if (collect) Some(collect)
          else None
        ) :: _traversal.steps)(target, st, t.et)
    }

    def coalesce[ET0 <: ClassType[_],
                 End1,
                 ET1 <: ClassType[End1],
                 Steps1 <: HList,
                 Steps2 <: HList,
                 Labels1 <: HList,
                 Labels2 <: HList](traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps1],
                                   traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps2])*)(
        implicit
        f1: Collect.Aux[Steps1, LabelSteps.type, Labels1],
        f2: Collect.Aux[Steps2, LabelSteps.type, Labels2],
        ev2: Labels1 =:= Labels2,
        et0: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Coalesce[ET[End], ET0] :: Steps] = {
      val coalesceTraversal = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal[ST[Start], ET1, Coalesce[ET[End], ET0] :: Steps](
        Coalesce[ET[End], ET0](
          coalesceTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]]()(target, et, et))
              .asInstanceOf[Traversal[ET[End], ET0, HList]])) :: _traversal.steps)(target, st, et0.ct)
    }

    def local[ET0 <: ClassType[_], Labels1 <: HList](
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList]) = {
      val localTraversal = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      Traversal[ST[Start], ET0, Local :: Steps](Local(localTraversal) :: _traversal.steps)(target,
                                                                                           st,
                                                                                           localTraversal.et)
    }

    /**
      * TODO: result of path is a List[ET0]
      * @param et0
      * @return
      */
    def path =
      Traversal(Path(Traversal[ET[End], ClassType[Any]]()(target, et, ClassType.default[Any])) :: _traversal.steps)(
        target,
        st,
        ClassType.default[Any])

    def path[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1], Steps0 <: HList](
        traversal: Traversal[ET[End], ClassType[Any], HNil] => Traversal[ET[End], ET0, Steps0])(
        implicit
        et0: ClassTypeable.Aux[ET0, End1, ET1]
    ): Traversal[ST[Start], ET1, Path :: Steps] = {
      val t = traversal(Traversal[ET[End], ClassType[Any]]()(target, et, ClassType.default[Any]))
      Traversal[ST[Start], ET1, Path :: Steps](Path(t) :: _traversal.steps)(target, st, et0.ct)
    }

    //    def is(value: End): Traversal[Start, Boolean, Is[End], Containers] =
    //      Traversal[Start, Boolean, Is[End], Containers](_traversal.step :+ Is(P.eq(value)))

    def is(predicate: P[End], predicates: P[_]*) =
      Traversal[ST[Start], ET[End], Is :: Steps](Is(predicate :: predicates.toList) :: _traversal.steps)(target, st, et)
  }

//  implicit class TravStepsHelper2[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
//      extends ResourceSteps[Start, ST, End, ET, Steps]
//      with FilterStepsHelper[Start, ST, End, ET, Steps]
//      with CommonStepsHelper[Start, ST, End, ET, Steps]
//      with MoveStepsHelper[Start, ST, End, ET, Steps]
//      with MoveMapStepsHelper[Start, ST, End, ET, Steps] {
//    implicit def target = _traversal.target
//
//    def id =
//      Traversal[ST[Start], LongType[Long], Id :: Steps](Id() :: _traversal.steps)
//  }
//  implicit class TravStepsEdgeHelper[Start, ST[+Z] <: ClassType[Z], EndS, EndE, ET[+Z] <: ClassType[Z],
//  Steps <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[Edge[EndS, EndE]], Steps])
//      extends ResourceSteps[Start, ST, Edge[EndS, EndE], ET, Steps]
//      with FilterStepsHelper[Start, ST, Edge[EndS, EndE], ET, Steps]
//      with CommonStepsHelper[Start, ST, Edge[EndS, EndE], ET, Steps]
//      with MoveStepsHelper[Start, ST, Edge[EndS, EndE], ET, Steps]
//      with MoveMapStepsHelper[Start, ST, Edge[EndS, EndE], ET, Steps] {
//    implicit def target = _traversal.target
//
//    def id =
//      Traversal[ST[Start], LongType[Long], Id :: Steps](Id() :: _traversal.steps)(target, st, DataType.default.longType)
//  }
  implicit class TravStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends ResourceSteps[Start, ST, End, ET, Steps]
      with FilterStepsHelper[Start, ST, End, ET, Steps]
      with CommonStepsHelper[Start, ST, End, ET, Steps]
      with MoveStepsHelper[Start, ST, End, ET, Steps]
      with MoveMapStepsHelper[Start, ST, End, ET, Steps] {
    implicit def target = _traversal.target

    def id =
      Traversal[ST[Start], LongType[Long], Id :: Steps](Id() :: _traversal.steps)(target, st, DataType.default.`@long`)

    def iri = _traversal.out(typed.iriUrlString)
  }

  implicit class NodeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[Node], Steps])
      extends StepsHelper[ST[Start], ET[Node], Steps] {
    implicit def target = _traversal.target

    def label(key: String, keys: String*): Traversal[ST[Start], IriType[Ontology], Label :: Steps] =
      label((key :: keys.toList).map(key => target.ns.getOntology(key).getOrElse(Ontology(key))): _*)
    def label(keys: List[Ontology] = List()): Traversal[ST[Start], IriType[Ontology], Label :: Steps] =
      label(keys: _*)
    def label(key: Ontology*): Traversal[ST[Start], IriType[Ontology], Label :: Steps] =
      Traversal[ST[Start], IriType[Ontology], Label :: Steps](Label(key.toSet) :: _traversal.steps)(
        target,
        st,
        DataType.default.ontologyURLType)
  }

  implicit class EdgeStepsHelper[Start, ST[+Z] <: ClassType[Z], In, Out, ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], Steps])
      extends StepsHelper[ST[Start], ET[Edge[In, Out]], Steps] {
    implicit val target = _traversal.target

    def outR[InC, InCT <: ClassType[InC]](
        implicit ct: ClassTypeable.Aux[In, InC, InCT]): Traversal[ST[Start], InCT, OutV :: Steps] = {
      Traversal[ST[Start], InCT, OutV :: Steps](OutV() :: _traversal.steps)(target, st, ct.ct)
    }
    def inR[OutC, OutCT <: ClassType[OutC]](
        implicit ct: ClassTypeable.Aux[Out, OutC, OutCT]): Traversal[ST[Start], OutCT, InV :: Steps] = {
      Traversal[ST[Start], OutCT, InV :: Steps](InV() :: _traversal.steps)(target, st, ct.ct)
    }

    def label(key: String, keys: String*): Traversal[ST[Start], IriType[Property], Label :: Steps] =
      label(
        (key :: keys.toList).map(
          key =>
            target.ns
              .getProperty(key)
              .getOrElse(Property(key))): _*)
    def label(keys: List[Property] = List()): Traversal[ST[Start], IriType[Property], Label :: Steps] =
      label(keys: _*)
    def label(key: Property*): Traversal[ST[Start], IriType[Property], Label :: Steps] =
      Traversal[ST[Start], IriType[Property], Label :: Steps](Label(key.toSet) :: _traversal.steps)(
        target,
        st,
        DataType.default.propertyURLType)
    //    def label(): Traversal[Start, Property, Label, Containers] =
    //      Traversal[Start, Property, Label, Containers](Label())
  }

  implicit class ValueStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])(implicit
                                                                            ev1: End =:!= Node,
                                                                            ev2: End =:!= Edge[_, _])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    def label[ET0 <: ET[End], End1, ET1 <: ClassType[End1]](keys: List[ET0] = List())(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Label :: Steps] =
      label(keys: _*)
    def label[ET0 <: ET[End], End1, ET1 <: ClassType[End1]](key: ET0*)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Label :: Steps] =
      Traversal(Label(key.toSet) :: _traversal.steps)(target, st, et.ct)
    //    def label(implicit dt: DataType[End]): Traversal[Start, DataType[End], Label, Containers] =
    //      Traversal[Start, DataType[End], Label, Containers](Label())
    def order(increasing: Boolean) =
      Traversal(Order(Traversal[ET[End], ET[End]]()(target, et, et), increasing) :: _traversal.steps)(target, st, et)
  }

  implicit class ResourceStepsHelper2[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])(implicit
                                                                            ev1: End =:!= Node,
                                                                            ev2: End =:!= Edge[_, _],
                                                                            ev3: End =:!= Value[_])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    //    def label(key: String, keys: String*): Traversal[ST[Start], ET[End], Label :: Steps] =
    //      label(
    //        (key :: keys.toList).map(
    //          key =>
    //            target.ns
    //              .getProperty(key)
    //              .orElse(MemGraphDefault.ns.getProperty(key))
    //              .map(_.asInstanceOf[ET])
    //              .getOrElse(Property(key).asInstanceOf[ET])): _*)
    //    def label[ET0 <: ET](keys: List[ET0] = List())(
    //        implicit et: ET0): Traversal[ST[Start], ET0, Label :: Steps] =
    //      label(keys: _*)
    //    def label[ET0 <: ET](key: ET0*)(implicit et: ET0): Traversal[ST[Start], ET0, Label :: Steps] =
    //      Traversal[ST[Start],ET0, Label :: Steps](Label(key.toSet) :: _traversal.steps)
    //    def label(implicit dt: DataType[End]): Traversal[Start, DataType[End], Label] =
    //      Traversal[Start, DataType[End], Label](Label())
  }

  implicit class NumericSteps[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    def sum() =
      Traversal[ST[Start], ET[End], Sum :: Steps](Sum() :: _traversal.steps)(target, st, et)
    def max() =
      Traversal[ST[Start], ET[End], Max :: Steps](Max() :: _traversal.steps)(target, st, et)
    def min() =
      Traversal[ST[Start], ET[End], Min :: Steps](Min() :: _traversal.steps)(target, st, et)
    def mean() =
      Traversal[ST[Start], DoubleType[Double], Mean :: Steps](Mean() :: _traversal.steps)(target,
                                                                                          st,
                                                                                          DataType.default.`@double`)
  }

  implicit class QuantitySteps[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    def sum() =
      Traversal[ST[Start], ET[End], Sum :: Steps](Sum() :: _traversal.steps)(target, st, et)
    def max() =
      Traversal[ST[Start], ET[End], Max :: Steps](Max() :: _traversal.steps)(target, st, et)
    def min() =
      Traversal[ST[Start], ET[End], Min :: Steps](Min() :: _traversal.steps)(target, st, et)
    def mean() =
      Traversal[ST[Start], ET[End], Mean :: Steps](Mean() :: _traversal.steps)(target, st, et)
  }

  abstract class TemporalSteps[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    //    def sum(): CalendarResultStep = CalendarResultStep()
    def max() =
      Traversal[ST[Start], ET[End], Max :: Steps](Max() :: _traversal.steps)(target, st, et)

    def min() =
      Traversal[ST[Start], ET[End], Min :: Steps](Min() :: _traversal.steps)(target, st, et)
  }

  implicit class InstantSteps[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: CalendarType[Z], Steps <: HList](
      _traversal: Traversal[ST[Start], ET[Instant], Steps])
      extends TemporalSteps(_traversal)
  implicit class LocalDateTimeSteps[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: CalendarType[Z], Steps <: HList](
      _traversal: Traversal[ST[Start], ET[LocalDateTime], Steps])
      extends TemporalSteps(_traversal)
  implicit class LocalTimeSteps[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: CalendarType[Z], Steps <: HList](
      _traversal: Traversal[ST[Start], ET[LocalTime], Steps])
      extends TemporalSteps(_traversal)
  implicit class LocalDateSteps[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: CalendarType[Z], Steps <: HList](
      _traversal: Traversal[ST[Start], ET[LocalDate], Steps])
      extends TemporalSteps(_traversal)

  implicit class GeoSteps[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    def mean() =
      Traversal[ST[Start], ET[End], Mean :: Steps](Mean() :: _traversal.steps)(target, st, et)
  }

  implicit class ClipSteps[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    def timeLimit(time: squants.time.Time) =
      Traversal[ST[Start], ET[End], TimeLimit :: Steps](TimeLimit(Some(time)) :: _traversal.steps)(target, st, et)
    def noTimeLimit() =
      Traversal[ST[Start], ET[End], TimeLimit :: Steps](TimeLimit() :: _traversal.steps)(target, st, et)

    def range(low: Int, high: Int) =
      Traversal(Range(low, high) :: _traversal.steps)(target, st, et)

    def limit(max: Int) =
      Traversal(Limit(max) :: _traversal.steps)(target, st, et)

    def tail(max: Int) =
      Traversal(Tail(max) :: _traversal.steps)(target, st, et)

    def order[T, CT[+Z] <: DataType[Z]: Orderable](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT[T], _ <: HList],
        increasing: Boolean = true) =
      Traversal(Order(by(Traversal[ET[End], ET[End]]()(target, et, et)), increasing) :: _traversal.steps)(target,
                                                                                                          st,
                                                                                                          et)

    def count() =
      Traversal(Count() :: _traversal.steps)(target, st, DataType.default.`@long`)
  }

  /**
    * TODO: implicits for select-steps are not resolved by Intellij/IDEA but compile as they should, any suggestions are welcome...
    * @param _traversal
    * @param reverse
    * @param f
    * @param lub
    * @param selector
    * @tparam ST
    * @tparam ET
    * @tparam Steps
    * @tparam RSteps
    * @tparam Labels
    * @tparam SelectorOut
    */
  implicit class WithAsAndSelectSteps[Start,
                                      ST[+Z] <: ClassType[Z],
                                      End,
                                      ET[+Z] <: ClassType[Z],
                                      Steps <: HList,
                                      Labels <: HList,
                                      SelectorOut <: Selector[_, HNil]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])(
      implicit
      //      val st: ST,
//          val reverse: Reverse.Aux[Steps, RSteps],
      val f: Collect.Aux[Steps, LabelSteps.type, Labels],
      lub: LUBConstraint[Labels, As[_, _]],
      val selector: SelectorSelecter.Aux[Labels, SelectorOut])
      extends StepsHelper[ST[Start], ET[End], Steps] {
    implicit val target = _traversal.target

    import shapeless.ops.hlist.{Selector => _, _}

    def select[RLabels <: HList, Types <: HList, End, End0, ET0 <: ClassType[_]](
        implicit
        reverse: Reverse.Aux[Labels, RLabels],
        ev: ToList[RLabels, As[_, _]],
        mapper: Mapper.Aux[LabelStepTypes.type, RLabels, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End0, ET0]): Traversal[ST[Start], ET0, Select[End] :: Steps] =
      Traversal[ST[Start], ET0, Select[End] :: Steps](
        Select[End](ev(reverse(f(_traversal.steps))).map(_.label.toString)) :: _traversal.steps)(target, st, et.ct)

    def select[SelectedLabels <: HList, RSelectedLabels <: HList, End, End0, ET0 <: ClassType[End0]](
        label: SelectorOut => Selection[SelectedLabels, End])(implicit
                                                              //      lub: LUBConstraint[SelectedLabels, As[_]],
                                                              reverse: Reverse.Aux[SelectedLabels, RSelectedLabels],
                                                              ev: ToList[RSelectedLabels, As[_, _]],
                                                              et: ClassTypeable.Aux[End, End0, ET0]) =
      Traversal[ST[Start], ET0, Select[End] :: Steps](Select[End](
        ev(reverse(label(selector(f(_traversal.steps))).labels)).map(_.label.toString)) :: _traversal.steps)(target,
                                                                                                             st,
                                                                                                             et.ct)
    implicitly[As[String, String] <:< As[_, String]]
    def select[A <: String, TypeA, OutA <: HList /* <: As[_, _]*/, End1, ET1 <: ClassType[_]](a: () => A)(
        implicit
//        sel: shapeless.ops.hlist.Selector[Labels, As[_, a.type]],
//        mapper: Mapper.Aux[LabelStepToKeyValueLabelStep.type, Labels, Types],
//        sel: shapeless.ops.hlist.Filter.Aux[Labels, As[_, name], OutA],
        sel: CoFilter.Aux[Labels, As[_, A], OutA],
//        fl: FilterA[a.type],
//        fa: Collect.Aux[Labels, FilterA[a.type], OutA :: HNil],
//        fa: CollectFirst.Aux[Labels, FilterA[a.type], OutA],
        //                                                      ev: ToList[Labels, As[_, _]],
        mapper: Mapper.Aux[LabelStepTypes.type, OutA, TypeA :: HNil],
        //                                                      tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[TypeA, End1, ET1]): Traversal[ST[Start], ET1, Select[OutA] :: Steps] =
      Traversal(
//        Select[OutA](List(fa(f(_traversal.steps)).runtimeList.head).map(_.asInstanceOf[As[_, _]].label)) :: _traversal.steps)(
        Select[OutA](List(a().toString)) :: _traversal.steps)(target, st, et.ct)
//
    /**
      * implicits are off, the result type is (OutA, OutA) ???
      * @param a
      * @param b
      * @param sela
      * @param selb
      * @param ev
      * @param mapper
      * @param tupler
      * @param et
      * @tparam A
      * @tparam B
      * @tparam OutA
      * @tparam OutB
      * @tparam OutATail
      * @tparam OutBTail
      * @tparam Types
      * @tparam End
      * @tparam End1
      * @tparam ET1
      * @return
      */
    def select[A <: String,
               B <: String,
               OutA,
               OutB,
               OutATail <: HList,
               OutBTail <: HList,
               Types <: HList,
               End,
               End1,
               ET1 <: ClassType[_]](a: () => A, b: () => B)(
        implicit
//        fla: FilterA[a.type],
//        flb: FilterA[b.type],
//        fa: CollectFirst.Aux[Labels, FilterA[a.type], OutA],
//        fb: CollectFirst.Aux[Labels, FilterA[b.type], OutB],
        sela: CoFilter.Aux[Labels, As[_, A], OutA :: OutATail],
        selb: CoFilter.Aux[Labels, As[_, B], OutB :: OutBTail],
        ev: ToList[OutA :: OutB :: HNil, As[_, _]],
        mapper: Mapper.Aux[LabelStepTypes.type, OutA :: OutB :: HNil, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End1, ET1]): Traversal[ST[Start], ET1, Select[End] :: Steps] =
      Traversal(Select[End](List(a().toString, b().toString)) :: _traversal.steps)(target, st, et.ct)
  }

  def apply[ST0 <: ClassType[_], ET0 <: ClassType[_]]()(target: Graph, st: ST0, et: ET0): Traversal[ST0, ET0, HNil] =
    apply[ST0, ET0, HNil](HNil)(target, st, et)

  def apply[ST0 <: ClassType[_], ET0 <: ClassType[_], Steps <: HList](
      steps0: Steps)(target0: Graph, st0: ST0, et0: ET0): Traversal[ST0, ET0, Steps] = {
    val node0 = DetachedGraph.nodes.create(ontology)

    steps0.runtimeList.reverse.asInstanceOf[List[Node]].foreach(node0.addOut(keys.step, _))

    new Traversal[ST0, ET0, Steps] {
      val steps         = steps0
      val self: Node    = node0
      val target: Graph = target0
      val st: ST0       = st0
      val et: ET0       = et0
    }
  }

  def apply[ST0 <: ClassType[_], ET0 <: ClassType[_]](
      value0: Node)(target0: Graph, st0: ST0, et0: ET0): Traversal[ST0, ET0, HList] = {
    val types = value0.labels
    val steps0 = value0.out(Traversal.keys.stepStep).foldLeft[HList](HNil) {
      case (hlist, node) => Step.toStep(node) :: hlist
    }

    new Traversal[ST0, ET0, HList] {
      val steps         = steps0
      val self: Node    = value0
      val target: Graph = target0
      val st: ST0       = st0
      val et: ET0       = et0
    }
  }

  def stepsToContainerStructure(steps: List[Step]): HList = HNil

  def getCT[ST <: ClassType[_],
            ET <: ClassType[_],
            Steps <: HList,
            RSteps <: HList,
            Containers <: HList,
            Out,
            CT <: ClassType[Out]](traversal: Traversal[ST, ET, Steps])(
      implicit
      reverse: Reverse.Aux[Steps, RSteps],
      f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
      lf: StructureCalculator.Aux[Containers, ET, Out, CT]): CT = {
    val ct =
      lf.convert(f(reverse(traversal.steps)), traversal.et)
    if (ct.iri.nonEmpty) MemGraphDefault.ns.storeClassType(ct)
    ct
  }

//  implicit class WithTraversalStream[Start,
//                                     ST[+Z] <: ClassType[Z],
//                                     End,
//                                     ET[+Z] <: ClassType[Z],
//                                     Steps <: HList,
//                                     RSteps <: HList,
//                                     Containers <: HList,
//                                     Out,
//                                     CT <: ClassType[Out]](val traversal: Traversal[ST[Start], ET[End], Steps])(
//      implicit
//      val reverse: Reverse.Aux[Steps, RSteps],
//      val f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
//      val lf: StructureCalculator.Aux[Containers, End, ET[End], Out, CT]) {
//
//    lazy val ct = getCT[Start, ST, End, ET, Steps, RSteps, Containers, Out, CT](traversal)(reverse, f, lf)
//
//    private[this] lazy val stream =
//      traversal.target.buildTraversersStream[ST[Start], ET[End], Steps, Out](traversal)(ct)
//    private[this] lazy val astream =
//      traversal.target.buildAsyncTraversersStream[ST[Start], ET[End], Steps, Out](traversal)(ct)
//
//    def iterate()    = stream.foreach(t => Unit)
//    def head         = stream.head
//    def headOption   = stream.headOption
//    def toList       = stream.toList
//    def toListSet    = stream.to[ListSet]
//    def toSet        = stream.toSet
//    def toStream     = stream
//    def toVector     = stream.toVector
//    def next(n: Int) = stream.take(n)
//    def toTask       = astream
//  }

  implicit class WithTraversalStream[ST <: ClassType[_],
                                     ET <: ClassType[_],
                                     Steps <: HList,
                                     RSteps <: HList,
                                     Containers <: HList,
                                     Out,
                                     CT <: ClassType[Out]](val traversal: Traversal[ST, ET, Steps])(
      implicit
      val reverse: Reverse.Aux[Steps, RSteps],
      val f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
      val lf: StructureCalculator.Aux[Containers, ET, Out, CT]) {

    lazy val ct = getCT[ST, ET, Steps, RSteps, Containers, Out, CT](traversal)(reverse, f, lf)

    private[this] lazy val stream =
      traversal.target.buildTraversersStream[ST, ET, Steps, Out](traversal)(ct)
    private[this] lazy val astream =
      traversal.target.buildAsyncTraversersStream[ST, ET, Steps, Out](traversal)(ct)

    def iterate()    = stream.foreach(t => Unit)
    def head         = stream.head
    def headOption   = stream.headOption
    def toList       = stream.toList
    def toListSet    = stream.to[ListSet]
    def toSet        = stream.toSet
    def toStream     = stream
    def toVector     = stream.toVector
    def next(n: Int) = stream.take(n)
    def toTask       = astream
  }
}

/**
  * TODO: try to convert End to shapeless.Coproduct
  * @param target
  * @param self
  * @param steps
  * @tparam Start
  * @tparam End
  * @tparam Steps
  */
trait Traversal[+ST <: ClassType[_], +ET <: ClassType[_], Steps <: HList] {
  def steps: Steps
  def self: Node
  def target: Graph
  def st: ST
  def et: ET

  lazy val stepsList: List[Step] = steps.runtimeList.asInstanceOf[List[Step]].reverse

  def toUntypedStream: Stream[Any] =
    target.buildTraversersStream[ST, DataType[Any], HNil, Any](this.asInstanceOf[Traversal[ST, DataType[Any], HNil]])(
      new DataType[Any] {
        val iri = ""
      })

  def withGraph(graph: Graph): Traversal[ST, ET, Steps] = {
    //    implicit val _target = target
    Traversal[ST, ET, Steps](steps)(target, st, et)
  }

  def prettyPrint: String = {
    stepsList.map(_.prettyPrint).mkString(".")
  }
}
