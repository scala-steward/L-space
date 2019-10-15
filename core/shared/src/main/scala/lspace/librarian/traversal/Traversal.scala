package lspace.librarian.traversal

import lspace.Label.D._
import lspace.Label.P._
import lspace.datatype._
import lspace.librarian.logic.predicate.P
import lspace.librarian.task._
import lspace.librarian.traversal.Traversal.keys
import lspace.structure.util.ClassTypeable
import lspace.librarian.traversal.step.Order.Orderable
import lspace.librarian.traversal.step.Select.Selection
import lspace.librarian.traversal.step._
import lspace.librarian.traversal.util.{EndMapper, LabelStepTypes, LabelSteps, ResultMapper, Selector, SelectorSelecter}
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.types.geo.Geometry
import lspace.util.types.DefaultsToAny
import monix.eval.{Coeval, Task}
import shapeless.{::, <:!<, =:!=, HList, HNil, LUBConstraint, Poly1, Id => _, Path => _, Select => _}
import shapeless.ops.hlist.{Collect, Prepend, Reverse}

import scala.annotation.{implicitNotFound, tailrec}

object Traversal
    extends OntologyDef(lspace.NS.vocab.Lspace.+("librarian/Traversal"), Set(), "Traversal", "A traversal .. ") {

  private val defaultdatatypestub = new DataType[Any] {
    override def iri: String = ""
  }
  def toTraversal(node: Node): Task[Traversal[ClassType[Any], ClassType[Any], _ <: HList]] = {
    val types = node.labels

    for {
      steps0 <- Task
        .gather(node.out(keys.stepsNode).take(1).flatMap(_.toList).map(Step.toStep))
        .map(_.foldLeft[HList](HNil) {
          case (hlist, step) => step :: hlist
        })
    } yield {
      new Traversal[ClassType[Any], ClassType[Any], HList](steps0)(ClassType.stubAny, ClassType.stubAny)
        .retype(ClassType.stubAny, ClassType.stubAny)
    }
  }

  object keys {

    object steps
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Traversal/steps",
          "steps",
          "The steps in a traversal",
          `@range` = VectorType(Step.ontology) :: Nil
        ) {}

    lazy val stepsNode: TypedProperty[Vector[Node]] = steps.property as VectorType(Step.ontology)
  }

  override lazy val properties: List[Property] = keys.steps :: Nil

  trait Properties {
    lazy val `ns.l-space.eu/librarian/Traversal/steps`: Property                    = keys.steps
    lazy val `ns.l-space.eu/librarian/Traversal/steps@Node`: TypedKey[Vector[Node]] = keys.stepsNode
  }

  implicit class TraversalMod[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends BaseMod[Start, ST, End, ET, Steps]
      with FilterStepsHelper[Start, ST, End, ET, Steps]
      with CommonStepsHelper[Start, ST, End, ET, Steps]
      with ClipStepsHelper[Start, ST, End, ET, Steps]
      with MoveStepsHelper[Start, ST, End, ET, Steps]
      with MoveMapStepsHelper[Start, ST, End, ET, Steps] {

    def G(graph: Graph*): Traversal[ST[Start], GraphType[Graph], step.G :: Steps] =
      Traversal[ST[Start], GraphType[Graph], G :: Steps](step.G(graph.toList) :: _traversal.steps)(st,
                                                                                                   GraphType.datatype)

    def N(): Traversal[ST[Start], NodeURLType[Node], step.N :: Steps] =
      add(step.N(), st, NodeURLType.datatype)

    def N(resource: Node, resources: Node*): Traversal[ST[Start], NodeURLType[Node], step.N :: Steps] =
      add(step.N(resource :: resources.toList), st, NodeURLType.datatype)

    def E: Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], step.E :: Steps] = //: Traversal[ST[Start],ClassType[Edge[S, E]], step.E :: Steps] =
      E[Any, Any]()

    def E[S: DefaultsToAny, E: DefaultsToAny](
        resource: Edge[S, E]*): Traversal[ST[Start], EdgeURLType[Edge[S, E]], step.E :: Steps] =
      add(step.E(resource.toList.asInstanceOf[List[Edge[Any, Any]]]), st, EdgeURLType.apply[Edge[S, E]])

    def V(): Traversal[ST[Start], DataType[Any], step.V :: Steps] =
      add(step.V(), st, DataType.datatype)

    def V[T, OutC, Out <: ClassType[OutC]](value: T, values: T*)(
        implicit cls: ClassTypeable.Aux[T, OutC, Out]): Traversal[ST[Start], Out, step.V :: Steps] =
      add(step.V(value :: values.toList), st, cls.ct)

    //FIX: within MoveStepsHelper the result type (ET1) freaks-out the compiler, it has trouble calculating it (IDEA shows correct types)
    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V])(
        implicit et: ClassTypeable.Aux[V, End1, ET1]): Traversal[ST[Start], ET1, HasLabel :: Out :: Steps] =
      add(Out(Set(key.key)), st, ClassType.stubAny)
        .hasLabel(et.asInstanceOf[ClassTypeable[V]])
        .asInstanceOf[Traversal[ST[Start], ET1, HasLabel :: Out :: Steps]]

    //    def R[T <: Resource[T]: DefaultsToAny]: Traversal[Start, T, step.R :: Steps] = R()
    //    def R[T <: Resource[T]: DefaultsToAny](value: T*): Traversal[Start, T, step.R :: Steps] =
    //      Traversal[Start, T, step.R :: Steps](step.R(value.toList.asInstanceOf[List[Resource[Any]]]) :: _traversal.steps)
  }

  trait BaseMod[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList] {

    protected[this] def _traversal: Traversal[ST[Start], ET[End], Steps]
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et

    def add[S <: Step](step: S) = new Traversal[ST[Start], ET[End], S :: Steps](step :: _traversal.steps)(st, et)
    def add[S <: Step, ST <: ClassType[Any], ET <: ClassType[Any]](step: S,
                                                                   st: ST,
                                                                   et: ET): Traversal[ST, ET, S :: Steps] =
      new Traversal(step :: _traversal.steps)(st, et)
  }

  trait FilterStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    implicit private def labelToProperty[L: HasStep.PropertyLabel](label: L): Property =
      label match {
        case label: Property    => label
        case label: PropertyDef => label.property
        case label: String =>
          Property.properties
            .get(label)
            .getOrElse(Property(label)) //throw new Exception("unknown key"))
      }

    //    def has[L: (String |∨| Property)#λ](label: L): Traversal[Start, End, Has :: Steps] = {
    def has[L: HasStep.PropertyLabel](label: L): Traversal[ST[Start], ET[End], Has :: Steps] =
      add(Has(label))
//    def has[L: HasStep.PropertyLabel, T <: HList](label: L, values: T)(
//        implicit d: LUBConstraint[T, P[_]],
//        ev: ToList[T, P[_]]): Traversal[ST[Start], ET[End], Has :: Steps] :: Segments] =
//      add(Has(label, values.toList))
    def has[L: HasStep.PropertyLabel, T](label: L, value: P[T]): Traversal[ST[Start], ET[End], Has :: Steps] =
      add(Has(label, Some(value)))

    def hasNot[L: HasStep.PropertyLabel](label: L): Traversal[ST[Start], ET[End], HasNot :: Steps] = add(HasNot(label))
    def hasNot[L: HasStep.PropertyLabel, T](label: L, value: P[T]): Traversal[ST[Start], ET[End], HasNot :: Steps] = {
      val step = HasNot(label, Some(value))
      add(step)
    }

    def hasId(id: Long, ids: Long*): Traversal[ST[Start], ET[End], HasId :: Steps] =
      add(HasId(id :: ids.toList toSet))
    def hasId(ids: Set[Long]): Traversal[ST[Start], ET[End], HasId :: Steps] = add(HasId(ids))

    /**
      * has at least one of the provided iris
      *
      * @param uris
      * @return
      */
    def hasIri(iri: String, uris: String*): Traversal[ST[Start], ET[End], HasIri :: Steps] =
      add(HasIri(iri :: uris.toList toSet))
    def hasIri(iris: Set[String]): Traversal[ST[Start], ET[End], HasIri :: Steps] =
      add(HasIri(iris))

    /**
      * has at least one of the provided labels
      *
      * @param label
      * @return
      */
    //    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1]](label0: ET0, labels: ET0*)(
    //        implicit et: ClassTypeable.Aux[ET0, End1, ET1]) = {
    //      Traversal[ST[Start], ET1, HasLabel :: Steps](HasLabel(label0 :: labels.toList) :: _traversal.steps)(target,
    //                                                                                                          st,
    //                                                                                                          et.ct)
    //    }

    def hasLabel(label: Ontology): Traversal[ST[Start], NodeURLType[Node], HasLabel :: Steps] =
      add(HasLabel(label :: Nil), st, NodeURLType.datatype)
    def hasLabel(label: Property): Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], HasLabel :: Steps] =
      add(HasLabel(label :: Nil), st, EdgeURLType.datatype)
    def hasLabel[T <: DataType[_], End1, ET1 <: ClassType[_]](label: T)(
        implicit et: ClassTypeable.Aux[T, End1, ET1]): Traversal[ST[Start], ET1, HasLabel :: Steps] =
      add(HasLabel(label :: Nil), st, et.ct)
//    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[_]](label0: ET0)(
//        implicit et: ClassTypeable.Aux[ET0, End1, ET1])
//      : Traversal[ST[Start], ET1, HasLabel :: Steps] =
//      add(HasLabel(label0 :: Nil), st, et.ct)
    def hasLabels[ET0 <: ClassType[_], End1, ET1 <: ClassType[_]](label0: ET0, label1: ET0)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, HasLabel :: Steps] =
      add(HasLabel(label0 :: label1 :: Nil), st, et.ct)
    def hasLabels[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[_]](label0: ET0[T],
                                                                         label1: ET0[T],
                                                                         label2: ET0[T])(
        implicit et: ClassTypeable.Aux[ET0[T], End1, ET1]): Traversal[ST[Start], ET1, HasLabel :: Steps] =
      add(HasLabel(label0 :: label1 :: label2 :: Nil), st, et.ct)
    def hasLabels[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[_]](label0: ET0[T],
                                                                         label1: ET0[T],
                                                                         label2: ET0[T],
                                                                         label3: ET0[T])(
        implicit et: ClassTypeable.Aux[ET0[T], End1, ET1]): Traversal[ST[Start], ET1, HasLabel :: Steps] =
      add(HasLabel(label0 :: label1 :: label2 :: label3 :: Nil), st, et.ct)
//    def hasLabel[T](implicit ev: T <:!< ClassType[_], label: _ <: ClassType[T]) = {
//      Traversal(HasLabel(label :: Nil) :: _traversal.steps)(target, st, label)
//    }
    def hasLabel[A](implicit cls: ClassTypeable[A]): Traversal[ST[Start], cls.CT, HasLabel :: Steps] =
      add(HasLabel(cls.ct :: Nil), st, cls.ct)
    //    def hasLabel[A,B](implicit clsA: ClassTypeable[A], clsB: ClassTypeable[B]) = {
    //      Traversal[ST[Start], cls.CT, HasLabel :: Steps](HasLabel(cls.ct :: Nil) :: _traversal.steps)(target, st, cls.ct)
    //    }

    //TODO: create IsNode step
//    def isNode: Traversal[ST[Start], NodeURLType[Node], HasLabel :: Steps] =
//      add(HasLabel())
//
    //TODO: create IsEdge step
//    def isEdge: Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], HasLabel :: Steps] =
//      add(HasLabel())

    def isNumber: Traversal[ST[Start], NumericType[AnyVal], HasLabel :: Steps] =
//      add(HasLabel(DataType.default.`@int` :: DataType.default.`@double` :: DataType.default.`@long` :: Nil),
      add(HasLabel(DataType.default.`@number` :: Nil), st, DataType.default.`@number`)

    def isTemporal: Traversal[ST[Start], CalendarType[Any], HasLabel :: Steps] =
//      add(HasLabel(DataType.default.`@datetime` :: DataType.default.`@datetime` :: DataType.default.`@time` :: Nil),
      add(HasLabel(DataType.default.`@temporal` :: Nil), st, DataType.default.`@temporal`)

    def isQuantity: Traversal[ST[Start], QuantityType[Any], HasLabel :: Steps] =
      add(HasLabel(DataType.default.`@quantity` :: Nil), st, DataType.default.`@quantity`)

    def isDuration: Traversal[ST[Start], DurationType[Any], HasLabel :: Steps] =
      add(HasLabel(DataType.default.`@duration` :: Nil), st, DataType.default.`@duration`)

    def isGeo: Traversal[ST[Start], GeometricType[Geometry], HasLabel :: Steps] =
      add(HasLabel(DataType.default.`@geo` :: Nil), st, DataType.default.`@geo`)

    def isColor: Traversal[ST[Start], ColorType[Any], HasLabel :: Steps] =
      add(HasLabel(DataType.default.`@color` :: Nil), st, DataType.default.`@color`)

    /**
      * A Coin step filters traversals based on a probability-distribution
      * @param p probability by which the traverser remains in the stream
      * @return
      */
    def coin(p: Double): Traversal[ST[Start], ET[End], Coin :: Steps] = add(Coin(p))
//    def coin(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], DoubleType[Double], _ <: HList])
//    : Traversal[ST[Start], ET[End], Coin :: Steps] =
//      add(Coin(traversal(Traversal[ET[End], ET[End]](et, et))))

    def constant[T, T0, TT0 <: ClassType[_]](p: T)(
        implicit ct: ClassTypeable.Aux[T, T0, TT0]): Traversal[ST[Start], TT0, Constant[T] :: Steps] =
      add(Constant(p)(ct.ct.asInstanceOf[ClassType[T]]), st, ct.ct)

    /**
      * Equal to Coin(0)
      * @return
      */
    def empty: Traversal[ST[Start], ET[End], Coin :: Steps] =
      coin(0)
  }

  trait MoveMapStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    def outMap(key: String, keys: String*): Traversal[ST[Start], MapType[Map[Property, List[Any]]], OutMap :: Steps] =
      add(
        OutMap(
          (key :: keys.toList)
            .map(
              key =>
                Property.properties
                  .get(key)
                  .getOrElse(Property(key)))
            .toSet),
        st,
        MapType(DataType.default.`@property`, ListType())
      )
    def outMap(keys: List[Property]): Traversal[ST[Start], MapType[Map[Property, List[Any]]], OutMap :: Steps] =
      add(OutMap(keys.toSet), st, MapType(DataType.default.`@property`, ListType()))
    def outMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Map[Property, List[Any]]], OutMap :: Steps] =
      add(OutMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
          st,
          MapType(DataType.default.`@property`, ListType()))
    def outMap(key: Property*): Traversal[ST[Start], MapType[Map[Property, List[Any]]], OutMap :: Steps] =
      add(OutMap(key.toSet), st, MapType(DataType.default.`@property`, ListType()))

    def outEMap(key: String,
                keys: String*): Traversal[ST[Start], MapType[Map[Property, List[Edge[End, Any]]]], OutEMap :: Steps] =
      add(
        OutEMap(
          (key :: keys.toList)
            .map(
              key =>
                Property.properties
                  .get(key)
                  .getOrElse(Property(key)))
            .toSet),
        st,
        MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[End, Any]]))
      )
    def outEMap(
        keys: List[Property]): Traversal[ST[Start], MapType[Map[Property, List[Edge[End, Any]]]], OutEMap :: Steps] =
      add(OutEMap(keys.toSet), st, MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[End, Any]])))
    def outEMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Map[Property, List[Edge[End, Any]]]], OutEMap :: Steps] =
      add(
        OutEMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
        st,
        MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[End, Any]]))
      )
    def outEMap(key: Property*): Traversal[ST[Start], MapType[Map[Property, List[Edge[End, Any]]]], OutEMap :: Steps] =
      add(OutEMap(key.toSet), st, MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[End, Any]])))

    def inMap(key: String, keys: String*): Traversal[ST[Start], MapType[Map[Property, List[Any]]], InMap :: Steps] =
      add(
        InMap(
          (key :: keys.toList)
            .map(
              key =>
                Property.properties
                  .get(key)
                  .getOrElse(Property(key)))
            .toSet),
        st,
        MapType(DataType.default.`@property`, ListType())
      )
    def inMap(keys: List[Property]): Traversal[ST[Start], MapType[Map[Property, List[Any]]], InMap :: Steps] =
      add(InMap(keys.toSet), st, MapType(DataType.default.`@property`, ListType()))
    def inMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Map[Property, List[Any]]], InMap :: Steps] =
      add(InMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
          st,
          MapType(DataType.default.`@property`, ListType()))
    def inMap(key: Property*): Traversal[ST[Start], MapType[Map[Property, List[Any]]], InMap :: Steps] =
      add(InMap(key.toSet), st, MapType(DataType.default.`@property`, ListType()))

    def inEMap(key: String,
               keys: String*): Traversal[ST[Start], MapType[Map[Property, List[Edge[Any, End]]]], InEMap :: Steps] =
      add(
        InEMap(
          (key :: keys.toList)
            .map(
              key =>
                Property.properties
                  .get(key)
                  .getOrElse(Property(key)))
            .toSet),
        st,
        MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[Any, End]]))
      )
    def inEMap(
        keys: List[Property]): Traversal[ST[Start], MapType[Map[Property, List[Edge[Any, End]]]], InEMap :: Steps] =
      add(InEMap(keys.toSet), st, MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[Any, End]])))
    def inEMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Map[Property, List[Edge[Any, End]]]], InEMap :: Steps] =
      add(
        InEMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
        st,
        MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[Any, End]]))
      )
    def inEMap(key: Property*): Traversal[ST[Start], MapType[Map[Property, List[Edge[Any, End]]]], InEMap :: Steps] =
      add(InEMap(key.toSet), st, MapType(DataType.default.`@property`, ListType(EdgeURLType.apply[Edge[Any, End]])))
  }

  trait MoveStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    def out(key: String, keys: String*) =
      add(Out(
            (key :: keys.toList)
              .map(
                key =>
                  Property.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          ClassType.stubAny)
    def out(keys: List[Property]) = add(Out(keys.toSet), st, ClassType.stubAny)
    def out(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(Out((f :: ff.toList).map(_.apply(Property.default)).toSet), st, ClassType.stubAny)
    def out(key: Property*) = add(Out(key.toSet), st, ClassType.stubAny)

//    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V])(implicit et: ClassTypeable.Aux[V, End1, ET1]) =
//      add(Out(Set(key.key)), st, ClassType.stubAny).hasLabel[V](et)
    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V], p: P[V])(
        implicit et: ClassTypeable.Aux[V, End1, ET1]) =
      add(Out(Set(key.key)), st, ClassType.stubAny).hasLabel(et).is(p.asInstanceOf[P[End1]])

    def outE(key: String, keys: String*) =
      add(OutE(
            (key :: keys.toList)
              .map(
                key =>
                  Property.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          EdgeURLType.apply[Edge[End, Any]])
    def outE(keys: List[Property]) = add(OutE(keys.toSet), st, EdgeURLType.apply[Edge[End, Any]])
    def outE(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(OutE((f :: ff.toList).map(_.apply(Property.default)).toSet), st, EdgeURLType.apply[Edge[End, Any]])
    def outE(key: Property*) = add(OutE(key.toSet), st, EdgeURLType.apply[Edge[End, Any]])

    def in(key: String, keys: String*) =
      add(In(
            (key :: keys.toList)
              .map(
                key =>
                  Property.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          ClassType.stubAny)
    def in(keys: List[Property]) = add(In(keys.toSet), st, ClassType.stubAny)
    def in(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(In((f :: ff.toList).map(_.apply(Property.default)).toSet), st, ClassType.stubAny)
    def in(key: Property*) = add(In(key.toSet), st, ClassType.stubAny)

    def inE(key: String, keys: String*) =
      add(InE(
            (key :: keys.toList)
              .map(
                key =>
                  Property.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          EdgeURLType.apply[Edge[Any, End]])
    def inE(keys: List[Property]) = add(InE(keys.toSet), st, EdgeURLType.apply[Edge[Any, End]])
    def inE(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(InE((f :: ff.toList).map(_.apply(Property.default)).toSet), st, EdgeURLType.apply[Edge[Any, End]])
    def inE(key: Property*) = add(InE(key.toSet), st, EdgeURLType.apply[Edge[Any, End]])

    def id = add(Id, st, DataType.default.`@long`)

    def iri =
      add(Out(Set(Property.default.`@id`)), st, ClassType.stubAny).hasLabel[String]
    //        .add(HasLabel(List(DataType.default.`@string`)), st, DataType.default.`@string`)
  }

  implicit class WithGroupStepHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], KOut,
  CK <: ClassType[_], KeySteps <: HList, Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start],
                                                TupleType[(KOut, List[End])],
                                                Group[CK, KeySteps, ET[End], HNil] :: Steps])
      extends BaseMod[Start, ST, (KOut, List[End]), TupleType, Group[CK, KeySteps, ET[End], HNil] :: Steps] {

//    def st: ST[Start]                    = _traversal.st
//    def et: TupleType[(KOut, List[End])] = _traversal.et
    def etV: ET[End] = _traversal.steps.head.value.et

    def mapValues[CV <: ClassType[Any], VSteps <: HList, Steps1 <: HList, VOut, CVOut <: ClassType[Any]](
        value: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CV, VSteps])(
        implicit
        prepend: Prepend.Aux[VSteps, Out :: HNil, Steps1], //Hack to trick OutTweaker with multi-librarian input (grouped librarians) //Out-step is to spoof OutTweaker to force a branched OutTweaker calculation
        outV: EndMapper.Aux[CV, Steps1, VOut, CVOut])
      : Traversal[ST[Start], TupleType[(KOut, VOut)], Group[CK, KeySteps, CV, VSteps] :: Steps] = {
      val step =
        Group[CK, KeySteps, CV, VSteps](_traversal.steps.head.by, value(Traversal(etV, etV)))
      Traversal[ST[Start], TupleType[(KOut, VOut)], Group[CK, KeySteps, CV, VSteps] :: Steps](
        step :: _traversal.steps.tail)(
        _traversal.st,
        TupleType[(KOut, VOut)](
          List(et.rangeTypes.head.asInstanceOf[Option[ClassType[KOut]]], Some(outV.map(step.value.et))))
      )
    }

//      Traversal[ST[Start], ET[End], Group[AZ, KeySegments, CV[V], ValueSegments] :: Steps](
//        _traversal.segments.head
//          .copy(Group[AZ, KeySegments, AZv, ValueSegments](
//            _traversal.segments.head.steps.head.by,
//            value(Traversal[ET[End], ET[End]](et, et))) :: _traversal.segments.head.steps.tail) :: _traversal.segments.tail)(
//        _traversal.st,
//        _traversal.et)
  }

  object CTOutMapper extends Poly1 {
    //    implicit def ct[T] = at[ClassType[T]](ct => 1.asInstanceOf[T])
    implicit def traversalC[End, ET[+Z] <: ClassType[Z]] = at[ET[End]](t => 1.asInstanceOf[End])
//    implicit def traversalMap[K, V]                      = at[MapType[K, V]](t => 1.asInstanceOf[Map[K, V]])
//    implicit def traversal[End] = at[ClassType[End]](t => 1.asInstanceOf[End])
  }

  implicit class WithProjectStepHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z],
  PST <: ClassType[Any], PET <: ClassType[Any], PHSteps <: HList, PROJECTIONS <: HList, Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start],
                                                ET[End],
                                                Project[Traversal[PST, PET, PHSteps] :: PROJECTIONS] :: Steps])
      extends BaseMod[Start, ST, End, ET, Project[Traversal[PST, PET, PHSteps] :: PROJECTIONS] :: Steps] {

    import util.{ProjectStepDataTypeMapper, ProjectStepTypeMapper}

    def by[P <: ClassType[Any],
           PSteps <: HList,
           ALLPROJECTIONS <: HList,
           Out <: HList,
           EndH <: HList,
           REndH <: HList,
           End0](value: Traversal[PST, PST, HNil] => Traversal[PST, P, PSteps])(
        implicit
        prepend: Prepend.Aux[Traversal[PST, P, PSteps] :: HNil,
                             Traversal[PST, PET, PHSteps] :: PROJECTIONS,
                             ALLPROJECTIONS],
        datatypeMapper: shapeless.ops.hlist.Mapper.Aux[ProjectStepDataTypeMapper.type, ALLPROJECTIONS, Out],
        typeMapper: shapeless.ops.hlist.Mapper.Aux[ProjectStepTypeMapper.type, ALLPROJECTIONS, EndH], //only for type-calculation, never executed
        reverse: shapeless.ops.hlist.Reverse.Aux[EndH, REndH],
        tupler: shapeless.ops.hlist.Tupler.Aux[REndH, End0] //only for type-calculation, never executed
    ): Traversal[ST[Start], TupleType[End0], Project[ALLPROJECTIONS] :: Steps] = {
      val step = Project[ALLPROJECTIONS](
        prepend(
          value(Traversal(_traversal.steps.head.by.head.st, _traversal.steps.head.by.head.st)) :: HNil,
          _traversal.steps.head.by
        ))
      Traversal[ST[Start], TupleType[End0], Project[ALLPROJECTIONS] :: Steps](step :: _traversal.steps.tail)(
        _traversal.st,
        TupleType[End0](
          datatypeMapper(step.by).runtimeList.reverse
            .asInstanceOf[List[ClassType[Any]]]
            .map(Some(_))
            .map(_.filter(_.iri.nonEmpty)))
      )
    }
  }

  trait CommonStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    def drop(): Traversal[ST[Start], ET[End], Drop :: Steps] = add(Drop)

    def dedup(): Traversal[ST[Start], ET[End], Dedup :: Steps] = add(Dedup)

    def as[S <: String](name: () => S): Traversal[ST[Start], ET[End], As[End, S] :: Steps] =
      add(As[End, S](name())(et))

    //TODO: add a 'byValue' traversal, so a traversal on the grouped result is contained within the step
    def group[CK <: ClassType[Any], KSteps <: HList, KOut, CKOut <: ClassType[Any]](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CK, KSteps])(
        implicit
        outK: EndMapper.Aux[CK, KSteps, KOut, CKOut]
    ): Traversal[ST[Start], TupleType[(KOut, List[End])], Group[CK, KSteps, ET[End], HNil] :: Steps] /*: Traversal[ST[Start],
                 TupleType[(KOut, List[End])],
                 Group[CK, KeySegments, ET[End], HNil] :: Steps]*/ = {

      val step =
        Group[CK, KSteps, ET[End], HNil](by(Traversal[ET[End], ET[End]](et, et)), Traversal[ET[End], ET[End]](et, et))
      add(
        step,
        st,
        TupleType[(KOut, List[End])](
          List(Some(outK.map(step.by.et)).filter(_.iri.nonEmpty), Some(ListType(step.value.et))))
      )
    }

    def project(): Traversal[ST[Start], ET[End], Project[Traversal[ET[End], ET[End], HNil] :: HNil] :: Steps] =
      add(Project(Traversal(et, et) :: HNil), st, et) //TupleType[End](List(Some(et))))

    def project[CP <: ClassType[Any], PSteps <: HList, POut, CPOut <: ClassType[Any]](
        by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CP, PSteps])(
        implicit
        out: EndMapper.Aux[CP, PSteps, POut, CPOut]
    ): Traversal[ST[Start], CPOut, Project[Traversal[ET[End], CP, PSteps] :: HNil] :: Steps] = {
      val tby1 = by1(Traversal(et, et))
      add(Project(tby1 :: HNil), st, out.map(tby1.et))
    }

    def where(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])
      : Traversal[ST[Start], ET[End], Where :: Steps] =
      add(Where(traversal(Traversal[ET[End], ET[End]](et, et))))

    def and(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
            traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*)
      : Traversal[ST[Start], ET[End], And :: Steps] =
      add(
        And(
          traversal(Traversal[ET[End], ET[End]](et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]](et, et)))))

    def or(
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
        traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*
    ): Traversal[ST[Start], ET[End], Or :: Steps] =
      add(
        Or(
          traversal(Traversal[ET[End], ET[End]](et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]](et, et)))))

    def not(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])
      : Traversal[ST[Start], ET[End], Not :: Steps] =
      add(Not(traversal(Traversal[ET[End], ET[End]](et, et))))

    def union[ET0 <: ClassType[_],
              End1,
              ET1 <: ClassType[End1],
              Steps1 <: HList,
              Steps2 <: HList
//              Labels2 <: HList
    ](traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps1],
      traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps2])*)(
        implicit
//        f1: Collect.Aux[Steps1, LabelSteps.type, Labels1],
//        f2: Collect.Aux[Steps2, LabelSteps.type, Labels2],
//        ev2: Labels1 =:= Labels2,
        et0: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Union[ET[End], ET0] :: Steps] = {
      val unionTraversal = traversal(Traversal[ET[End], ET[End]](et, et))
      add(
        Union[ET[End], ET0](
          unionTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]](et, et))
              .asInstanceOf[Traversal[ET[End], ET0, Steps1]])),
        st,
        et0.ct
      )
    }

    /**
      *
      * @param traversal to be repeated
      * @param until     nonempty traversal is being repeated
      * @param max       number of times is being repeated
      * @param collect   result of each loop
      * @return
      */
    def repeat[ET0 <: ClassType[_]](traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList],
                                    max: Int = 0,
                                    collect: Boolean = false,
                                    noloop: Boolean = false)(
        implicit until: Traversal[ET0, ET0, HNil] => Traversal[ET0, _ <: ClassType[_], _ <: HList] = null
    ): Traversal[ST[Start], ET0, Repeat[ET0] :: Steps] = {
      val t = traversal(Traversal[ET[End], ET[End]](et, et))
      add(
        Repeat(
          t,
          Option(until).map(_(Traversal[ET0, ET0](t.et, t.et))),
          if (max == 0) None else Some(max),
          collect,
          noloop
        ),
        st,
        t.et
      )
    }

    def choose[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1], Steps1 <: HList, Steps2 <: HList](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
        right: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps1],
        left: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps2])(
        implicit
//        f1: Collect.Aux[Steps1, LabelSteps.type, Labels1],
//        f2: Collect.Aux[Steps2, LabelSteps.type, Labels2],
//        ev2: Labels1 =:= Labels2,
        et0: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Choose[ET[End], ET0] :: Steps] = {
      val byTraversal    = by(Traversal[ET[End], ET[End]](et, et))
      val rightTraversal = right(Traversal[ET[End], ET[End]](et, et))
      val leftTraversal  = left(Traversal[ET[End], ET[End]](et, et))
      add(
        Choose[ET[End], ET0](byTraversal, rightTraversal, leftTraversal),
        st,
        et0.ct
      )
    }

    //TODO: HList for Coalesce traversals
    def coalesce[ET0 <: ClassType[Any], End1, ET1 <: ClassType[End1], Steps1 <: HList, Steps2 <: HList](
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps1],
        traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps2])*)(
        implicit
//        f1: Collect.Aux[Steps1, LabelSteps.type, Labels1],
//        f2: Collect.Aux[Steps2, LabelSteps.type, Labels2],
//        ev2: Labels1 =:= Labels2,
        et0: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Coalesce[ET[End], ET0] :: Steps] = {
      val coalesceTraversal = traversal(Traversal[ET[End], ET[End]](et, et))
      add(
        Coalesce[ET[End], ET0](
          coalesceTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]](et, et))
              .asInstanceOf[Traversal[ET[End], ET0, HList]])),
        st,
        et0.ct
      )
    }

    def local[ET0 <: ClassType[_], Labels1 <: HList](
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList])
      : Traversal[ST[Start], ET0, Local[ET[End], ET0] :: Steps] = {
      val localTraversal = traversal(Traversal[ET[End], ET[End]](et, et))
      add(Local(localTraversal), st, localTraversal.et)
    }

    /**
      * TODO: result of path is a List[ET0]
      *
      * @return
      */
    def path: Traversal[ST[Start], ListType[List[Any]], Path[ClassType[Any], HNil] :: Steps] =
      add(Path(Traversal[ET[End], ListType[List[Any]]](et, ListType(ClassType.stubAny))),
          st,
          ListType(ClassType.stubAny))

    def path[ET0 <: ClassType[Any], Steps0 <: HList, POut, CPOut <: ClassType[Any]](
        traversal: Traversal[ET[End], ClassType[Any], HNil] => Traversal[ET[End], ET0, Steps0])(
        implicit
        out: EndMapper.Aux[ET0, Steps0, POut, CPOut]
    ): Traversal[ST[Start], ListType[List[POut]], Path[ET0, Steps0] :: Steps] = {
      val t = traversal(Traversal(et, et))
      add(Path(t), st, ListType(out.map(t.et.asInstanceOf[ET0]).asInstanceOf[ClassType[POut]]))
    }

//    def is[T, T0, TT0 <: ClassType[_]](value: T)(
//        implicit ev: T <:!< P[_],
//        ct: ClassTypeable.Aux[T, T0, TT0]): Traversal[ST[Start], ET[End], Is :: Steps] =
//      is(P.eqv(value)(ct))
    def is(predicate: P[End]): Traversal[ST[Start], ET[End], Is :: Steps] =
      add(Is(predicate))
  }

  trait ClipStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    /**
      *
      * @param time in millis
      * @return
      */
    def timeLimit(time: Long): Traversal[ST[Start], ET[End], TimeLimit :: Steps] =
      add(TimeLimit(Some(time)))

    def noTimeLimit(): Traversal[ST[Start], ET[End], TimeLimit :: Steps] =
      add(TimeLimit())

    def range(low: Int, high: Int): Traversal[ST[Start], ET[End], Range :: Steps] =
      add(Range(low, high))

    def head(): Traversal[ST[Start], ET[End], Head :: Steps]           = add(Head)
    def last(): Traversal[ST[Start], ET[End], Last :: Steps]           = add(Last)
    def limit(max: Int): Traversal[ST[Start], ET[End], Limit :: Steps] = add(Limit(max))
    def skip(n: Int): Traversal[ST[Start], ET[End], Skip :: Steps]     = add(Skip(n))
    def tail(max: Int): Traversal[ST[Start], ET[End], Tail :: Steps]   = add(Tail(max))

    /**
      * sorts with the value of the first result
      * @param by
      * @tparam CT
      * @return
      */
    def order[CT <: DataType[_]: Orderable](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList],
        increasing: Boolean = true): Traversal[ST[Start], ET[End], Order :: Steps] = {
      val byTraversal = by(Traversal[ET[End], ET[End]](et, et))
      add(Order(byTraversal, increasing))
    }

    /**
      * compares to the value of the first result
      * @param by
      * @tparam CT
      * @return
      */
    def max[CT <: DataType[_]: Orderable](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList])
      : Traversal[ST[Start], ET[End], Max :: Steps] = {
      val byTraversal = by(Traversal[ET[End], ET[End]](et, et))
      add(Max(byTraversal))
    }

    /**
      * compares to the value of the first result
      * @param by
      * @tparam CT
      * @return
      */
    def min[CT <: DataType[_]: Orderable](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList])
      : Traversal[ST[Start], ET[End], Min :: Steps] = {
      val byTraversal = by(Traversal[ET[End], ET[End]](et, et))
      add(Min(byTraversal))
    }

    def count(): Traversal[ST[Start], LongType[Long], Count :: Steps] =
      add(Count: Count, st, DataType.default.`@long`)
  }

  implicit class WithNodeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[Node], Steps])
      extends BaseMod[Start, ST, Node, ET, Steps]
      with NodeStepsHelper[Start, ST, ET, Steps] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[Node]  = _traversal.et
  }
//  implicit class WithNodeStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[Node], HNil])
//      extends TModHNil[Start, ST, Node, ET]
//      with NodeStepsHelper[Start, ST, ET, HNil, HNil, HNil] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[Node]  = _traversal.et
//  }
  trait NodeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList]
      extends BaseMod[Start, ST, Node, ET, Steps] {

    /**
      * this looks redundant w.r.t. the global FilterStepsHelper, but somehow a 'hasLabel' definition in NodeStepsHelper overwrites all other definitions... :S
      * @param label
      * @return
      */
    def hasLabel(label: Ontology): Traversal[ST[Start], NodeURLType[Node], HasLabel :: Steps] =
      add(HasLabel(label :: Nil), st, NodeURLType.datatype)
    def hasLabel(label0: String, labels0: String*): Traversal[ST[Start], ET[Node], HasLabel :: Steps] = {
      val labelKeys = (label0 :: labels0.toList).map(key => Ontology.ontologies.get(key).getOrElse(Ontology(key)))
      add(HasLabel(labelKeys))
    }

    def label(key: String, keys: String*): Traversal[ST[Start], IriType[Ontology], Label :: Steps] =
      add(Label((key :: keys.toList).map(key => Ontology.ontologies.get(key).getOrElse(Ontology(key))).toSet),
          st,
          DataType.default.`@class`)
    def label(keys: List[Ontology] = List()): Traversal[ST[Start], IriType[Ontology], Label :: Steps] =
      add(Label(keys.toSet), st, DataType.default.`@class`)
    def label(key: Ontology*): Traversal[ST[Start], IriType[Ontology], Label :: Steps] =
      add(Label(key.toSet), st, DataType.default.`@class`)
  }

  implicit class WithEdgeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: EdgeURLType[Z], Steps <: HList, In, Out](
      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], Steps])
      extends BaseMod[Start, ST, Edge[In, Out], ET, Steps]
      with EdgeStepsHelper[Start, ST, ET, Steps, In, Out] {

//    def st: ST[Start]         = _traversal.st
//    def et: ET[Edge[In, Out]] = _traversal.et
  }
//  implicit class WithEdgeStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: EdgeURLType[Z], In, Out](
//      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], HNil])
//      extends TModHNil[Start, ST, Edge[In, Out], ET]
//      with EdgeStepsHelper[Start, ST, ET, HNil, HNil, HNil, In, Out] {
//
//    def st: ST[Start]         = _traversal.st
//    def et: ET[Edge[In, Out]] = _traversal.et
//  }
  trait EdgeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList, In, Out]
      extends BaseMod[Start, ST, Edge[In, Out], ET, Steps] {

    def from[InC, InCT <: ClassType[InC]](
        implicit ct: ClassTypeable.Aux[In, InC, InCT]): Traversal[ST[Start], InCT, From :: Steps] = {
      add(From: From, st, ct.ct)
    }

    def to[OutC, OutCT <: ClassType[OutC]](
        implicit ct: ClassTypeable.Aux[Out, OutC, OutCT]): Traversal[ST[Start], OutCT, To :: Steps] = {
      add(To: To, st, ct.ct)
    }

    /**
      * this looks redundant w.r.t. the global FilterStepsHelper, but somehow a 'hasLabel' definition in EdgeStepsHelper overwrites all other definitions... :S
      * @param label
      * @return
      */
    def hasLabel(label: Property): Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], HasLabel :: Steps] =
      add(HasLabel(label :: Nil), st, EdgeURLType.datatype)
    def hasLabel(label0: String, labels0: String*): Traversal[ST[Start], ET[Edge[In, Out]], HasLabel :: Steps] = {
      val labelKeys = (label0 :: labels0.toList).map(key => Property.properties.get(key).getOrElse(Property(key)))
      add(HasLabel(labelKeys))
    }

    def label(key: String, keys: String*): Traversal[ST[Start], IriType[Property], Label :: Steps] =
      add(Label(
            (key :: keys.toList)
              .map(
                key =>
                  Property.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          DataType.default.`@property`)

    def label(keys: List[Property] = List()): Traversal[ST[Start], IriType[Property], Label :: Steps] =
      add(Label(keys.toSet), st, DataType.default.`@property`)

    def label(key: Property*): Traversal[ST[Start], IriType[Property], Label :: Steps] =
      add(Label(key.toSet), st, DataType.default.`@property`)
  }

  implicit class WithValueStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])(implicit ev: ET[End] <:!< IriType[End])
      extends BaseMod[Start, ST, End, ET, Steps]
      with ValueStepsHelper[Start, ST, End, ET, Steps] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
  }
//  implicit class WithValueStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])(implicit ev: ET[End] <:!< IriType[End])
//      extends TModHNil[Start, ST, End, ET]
//      with ValueStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
//  }
  trait ValueStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    def label[ET0 <: ET[End], End1, ET1 <: ClassType[End1]](keys: List[ET0] = List())(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Label :: Steps] =
      add(Label(keys.toSet), st, et.ct)

    def label[ET0 <: ET[End], End1, ET1 <: ClassType[End1]](key: ET0*)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1]): Traversal[ST[Start], ET1, Label :: Steps] =
      add(Label(key.toSet), st, et.ct)
  }

  implicit class WithNumericStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends BaseMod[Start, ST, End, ET, Steps]
      with NumericStepsHelper[Start, ST, End, ET, Steps] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
  }
//  implicit class WithNumericStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
//      extends TModHNil[Start, ST, End, ET]
//      with NumericStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
//  }
  trait NumericStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

//    def sum(): Traversal[ST[Start], ET[End], Sum :: Steps] = add(Sum)
    def sum(): Traversal[ST[Start], DoubleType[Double], Sum :: Steps] =
      add(Sum: Sum, st, `@double`)
    def max(): Traversal[ST[Start], ET[End], Max :: Steps] =
      add(Max(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def min(): Traversal[ST[Start], ET[End], Min :: Steps] =
      add(Min(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def mean(): Traversal[ST[Start], DoubleType[Double], Mean :: Steps] =
      add(Mean: Mean, st, DataType.default.`@double`)
    def order[ET0 <: DataType[_]](increasing: Boolean = true): Traversal[ST[Start], ET[End], Order :: Steps] =
      add(Order(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))

  }

  implicit class WithQuantityStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends BaseMod[Start, ST, End, ET, Steps]
      with QuantityStepsHelper[Start, ST, End, ET, Steps] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
  }
//  implicit class WithQuantityStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
//      extends TModHNil[Start, ST, End, ET]
//      with QuantityStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
//  }
  trait QuantityStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    def sum(): Traversal[ST[Start], ET[End], Sum :: Steps] = add(Sum: Sum)
    def max(): Traversal[ST[Start], ET[End], Max :: Steps] =
      add(Max(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def min(): Traversal[ST[Start], ET[End], Min :: Steps] =
      add(Min(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def mean(): Traversal[ST[Start], ET[End], Mean :: Steps] = add(Mean: Mean)
    def order[ET0 <: DataType[_]](increasing: Boolean = true): Traversal[ST[Start], ET[End], Order :: Steps] =
      add(Order(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))
  }

  implicit class WithTemporalStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends BaseMod[Start, ST, End, ET, Steps]
      with TemporalStepsHelper[Start, ST, End, ET, Steps] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
  }
//  implicit class WithTemporalStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
//      extends TModHNil[Start, ST, End, ET]
//      with TemporalStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
//  }
  trait TemporalStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    //    def sum(): CalendarResultStep = CalendarResultStep()
    def max(): Traversal[ST[Start], ET[End], Max :: Steps] =
      add(Max(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def min(): Traversal[ST[Start], ET[End], Min :: Steps] =
      add(Min(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def order[ET0 <: DataType[_]](increasing: Boolean = true): Traversal[ST[Start], ET[End], Order :: Steps] =
      add(Order(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))
  }

//  implicit class InstantSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[Instant], Steps] :: Segments])
//      extends TemporalSteps(_traversal)
//
//  implicit class LocalDateTimeSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[LocalDateTime], Steps] :: Segments])
//      extends TemporalSteps(_traversal)
//
//  implicit class LocalTimeSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[LocalTime], Steps] :: Segments])
//      extends TemporalSteps(_traversal)
//
//  implicit class LocalDateSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[LocalDate], Steps] :: Segments])
//      extends TemporalSteps(_traversal)

  implicit class WithGeoStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z], Steps <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps])
      extends BaseMod[Start, ST, End, ET, Steps]
      with GeoStepsHelper[Start, ST, End, ET, Steps] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
  }
//  implicit class WithGeoStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
//      extends TModHNil[Start, ST, End, ET]
//      with GeoStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
//  }
  trait GeoStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z], Steps <: HList]
      extends BaseMod[Start, ST, End, ET, Steps] {

    def mean(): Traversal[ST[Start], ET[End], Mean :: Steps] = add(Mean: Mean)
  }

//  object SegmentMapper extends Poly1 {
//    implicit def getSteps[Steps <: HList] = at[Steps]](s => s.steps)
//  }

  /**
    * TODO: implicits for select-steps are not resolved by Intellij/IDEA but compile as they should, any suggestions are welcome...
    *
    * @param _traversal
    * @param f
    * @param lub
    * @param selector
    * @tparam Start
    * @tparam ST
    * @tparam End
    * @tparam ET
    * @tparam Steps
    * @tparam Labels
    * @tparam SelectorOut
    */
//    implicit class WithAsAndSelectSteps[AllSteps <: HList, Labels <: HList, SelectorOut <: Selector[_, HNil]](
//        protected[this] val _traversal: Traversal[ST[Start], ET[End], Steps] :: Segments])(
//        implicit
//        //      val st: ST,
//        //          val reverse: Reverse.Aux[Steps, RSteps],
//        val f: Collect.Aux[AllSteps, LabelSteps.type, Labels],
//        lub: LUBConstraint[Labels, As[_, _]],
//        val selector: SelectorSelecter.Aux[Labels, SelectorOut])
//        extends StepsHelper[ST[Start], ET[End]] {
  implicit class WithAsAndSelectStepsHelper[Start,
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
      protected val f: Collect.Aux[Steps, LabelSteps.type, Labels],
      protected val lub: LUBConstraint[Labels, As[_, _]],
      protected val selector: SelectorSelecter.Aux[Labels, SelectorOut])
      extends BaseMod[Start, ST, End, ET, Steps]
      with AsAndSelectStepsHelper[Start, ST, End, ET, Steps, Labels, SelectorOut] {

//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
  }

//  implicit class WithAsAndSelectStepsHelperHNil[Start,
//                                                ST[+Z] <: ClassType[Z],
//                                                End,
//                                                ET[+Z] <: ClassType[Z],
//                                                Steps <: HList,
//                                                Labels <: HList,
//                                                SelectorOut <: Selector[_, HNil]](
//      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])(
//      implicit
//      //      val st: ST,
//      //          val reverse: Reverse.Aux[Steps, RSteps],
//      protected val flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, HNil, Steps],
//      protected val f: Collect.Aux[Steps, LabelSteps.type, Labels],
//      protected val lub: LUBConstraint[Labels, As[_, _]],
//      protected val selector: SelectorSelecter.Aux[Labels, SelectorOut])
//      extends TModHNil[Start, ST, End, ET]
//      with AsAndSelectStepsHelper[Start, ST, End, ET, HNil, HNil, HNil, Steps, Labels, SelectorOut] {
//
//    def st: ST[Start] = _traversal.st
//    def et: ET[End]   = _traversal.et
//  }

  trait AsAndSelectStepsHelper[Start,
                               ST[+Z] <: ClassType[Z],
                               End,
                               ET[+Z] <: ClassType[Z],
                               Steps <: HList,
                               Labels <: HList,
                               SelectorOut <: Selector[_, HNil]]
      extends BaseMod[Start, ST, End, ET, Steps] {

    protected def f: Collect.Aux[Steps, LabelSteps.type, Labels]
    protected def lub: LUBConstraint[Labels, As[_, _]]
    protected def selector: SelectorSelecter.Aux[Labels, SelectorOut]

//    protected[this] def _traversal: Traversal[ST[Start], ET[End], Steps]
    import shapeless.ops.hlist.{Selector => _, _}

    def select[RLabels <: HList, Types <: HList, End, End0, ET0 <: ClassType[_]](
        implicit
        reverse: Reverse.Aux[Labels, RLabels],
        ev: ToList[RLabels, As[_, _]],
        mapper: shapeless.ops.hlist.Mapper.Aux[LabelStepTypes.type, RLabels, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End0, ET0]): Traversal[ST[Start], ET0, Select[End] :: Steps] =
      add(Select[End](ev(reverse(f(_traversal.steps))).map(_.label.toString)), st, et.ct)

    def select[SelectedLabels <: HList, RSelectedLabels <: HList, End, End0, ET0 <: ClassType[End0]](
        label: SelectorOut => Selection[SelectedLabels, End])(implicit
                                                              //      lub: LUBConstraint[SelectedLabels, As[_]],
                                                              reverse: Reverse.Aux[SelectedLabels, RSelectedLabels],
                                                              ev: ToList[RSelectedLabels, As[_, _]],
                                                              et: ClassTypeable.Aux[End, End0, ET0]) =
      add(Select[End](ev(reverse(label(selector(f(_traversal.steps))).labels)).map(_.label.toString)), st, et.ct)

    def select[A <: String, TypeA, OutA <: HList, End1, ET1 <: ClassType[_]](a: () => A)(
        implicit
        sel: CoFilter.Aux[Labels, As[_, A], OutA],
        mapper: shapeless.ops.hlist.Mapper.Aux[LabelStepTypes.type, OutA, TypeA :: HNil],
        et: ClassTypeable.Aux[TypeA, End1, ET1]): Traversal[ST[Start], ET1, Select[OutA] :: Steps] =
      add(Select[OutA](List(a())), st, et.ct)

    //
    /**
      * implicits are off, the result type is (OutA, OutA) ???
      *
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
        sela: CoFilter.Aux[Labels, As[_, A], OutA :: OutATail],
        selb: CoFilter.Aux[Labels, As[_, B], OutB :: OutBTail],
        ev: ToList[OutA :: OutB :: HNil, As[_, _]],
        mapper: shapeless.ops.hlist.Mapper.Aux[LabelStepTypes.type, OutA :: OutB :: HNil, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End1, ET1]): Traversal[ST[Start], ET1, Select[End] :: Steps] =
      add(Select[End](List(a(), b())), st, et.ct)
  }

  def apply[S: DefaultsToAny, E: DefaultsToAny]()(
      implicit cltblStart: ClassTypeable[S],
      cltblEnd: ClassTypeable[E]): Traversal[cltblStart.CT, cltblEnd.CT, HNil] =
    new Traversal(HNil: HNil)(cltblStart.ct, cltblEnd.ct)
  def apply[ST0 <: ClassType[_], ET0 <: ClassType[_]](st: ST0, et: ET0): Traversal[ST0, ET0, HNil] =
    new Traversal(HNil: HNil)(st, et)

  implicit class WithTraversalStreamTyped[ST <: ClassType[Any], ET <: ClassType[Any], Steps <: HList](
      val traversal: Traversal[ST, ET, Steps]) {

    @implicitNotFound("could not find a Guide or could not build the result type")
    def withGraph[Out, OutCT <: ClassType[Any], F[_]](graph: Graph)(implicit
                                                                    tweaker: EndMapper.Aux[ET, Steps, Out, OutCT],
                                                                    guide: Guide[F],
                                                                    mapper: ResultMapper[F, ET, OutCT]): mapper.FT =
      mapper
        .apply(traversal.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]], graph)
        .asInstanceOf[mapper.FT]
  }

  def apply(steps: Vector[Step]): Traversal[ClassType[Any], ClassType[Any], _ <: HList] = {
    import scala.collection.immutable.::

    Traversal(steps.foldLeft[HList](HNil) {
      case (steps, step) => step :: steps
    })(ClassType.stubAny, ClassType.stubAny).retype()
  }

  @tailrec
  def stepsToTraversal(steps: Vector[Step], traversal: Traversal[ClassType[Any], ClassType[Any], _ <: HList])
    : Traversal[ClassType[Any], ClassType[Any], _ <: HList] = {
    steps match {
      case Vector() => traversal
      case (step: TraverseStep) +: steps =>
        val typedTraversal = step match {
          case step: Constant[_] =>
            new Traversal(step :: traversal.steps)(traversal.st, step.label)
          case step: From => //TODO: create EdgeUrlType with From and To type information
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
          case step: To => //TODO: create EdgeUrlType with From and To type information
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
          case step: Id => new Traversal(step :: traversal.steps)(traversal.st, `@long`)
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: ResourceStep) +: steps =>
        val typedTraversal = step match {
          case step: N => new Traversal(step :: traversal.steps)(traversal.st, Node.nodeUrl)
          case step: E => new Traversal(step :: traversal.steps)(traversal.st, Edge.edgeUrl)
          case step: V =>
            new Traversal(step :: traversal.steps)(traversal.st, ValueURLType.datatype)
          case step: R =>
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
          case step: GraphStep =>
            new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: BranchStep) +: steps =>
        val typedTraversal = step match {
          case step: BranchStep =>
            step match {
              case step: MoveStep =>
                step match {
                  case step: Out =>
                    new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
                  case step: OutE =>
                    new Traversal(step :: traversal.steps)(traversal.st, Edge.edgeUrl)
                  case step: In =>
                    new Traversal(step :: traversal.steps)(traversal.st, ClassType.stubAny)
                  case step: InE =>
                    new Traversal(step :: traversal.steps)(traversal.st, Edge.edgeUrl)
                }
              case step: Choose[_, _] =>
                val typedStep = Choose(
                  step.by.retype(traversal.et, traversal.et),
                  step.right.retype(traversal.et, traversal.et),
                  step.left.retype(traversal.et, traversal.et)
                )
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.right.et + typedStep.left.et)
              case step: Coalesce[_, _] =>
                val typedStep = Coalesce(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversals.map(_.et).reduce(_ + _))
              case step: Local[_, _] =>
                val typedStep = Local(step.traversal.retype(traversal.et, traversal.et))
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversal.et)
              case step: Repeat[_] =>
                val typedStep = Repeat(step.traversal.retype(traversal.et, traversal.et),
                                       step.until.map(_.retype(step.traversal.et, step.traversal.et)),
                                       step.max,
                                       step.collect,
                                       step.noloop)
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversal.et)
              case step: Union[_, _] =>
                val typedStep = Union(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
                new Traversal(typedStep :: traversal.steps)(traversal.st, typedStep.traversals.map(_.et).reduce(_ + _))
            }
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: ProjectionStep) +: steps =>
        val typedTraversal = step match {
          case step: MapStep =>
            new Traversal(step :: traversal.steps)(traversal.st,
                                                   MapType(DataType.default.`@property`, ListType(ClassType.stubAny)))
          case step: Select[_] =>
            val labels = traversal.stepsList.collect { case step: As[_, _] => step }
            val types  = step.names.map(name => labels.find(_.label == name).map(_.ct))
            new Traversal(step :: traversal.steps)(traversal.st, TupleType(types))
          case step: Project[_] =>
            val typedStep = Project(
              step.by.runtimeList.reverse
                .map {
                  case t: Traversal[_, _, _] =>
                    t.retype(traversal.et, traversal.et)
                }
                .foldLeft[HList](HNil) { case (r, t) => t :: r })
            val typedProjections = typedStep.by.runtimeList
            val et = typedProjections.lengthCompare(1) match {
              case -1 => traversal.et
              case 0  => typedProjections.headOption.map { case t: Traversal[_, _, _] => t.enclosedEndType }.get
              case 1 =>
                TupleType(
                  typedProjections.reverse
                    .map { case t: Traversal[_, _, _] => t.enclosedEndType }
                    .map(Some(_)))
            }
            new Traversal(typedStep :: traversal.steps)(traversal.st, et)
          case step: Path[_, _] =>
            val typedStep = Path(step.by.retype(traversal.et, traversal.et))
            new Traversal(typedStep :: traversal.steps)(traversal.st, ListType[Any](typedStep.by.et))
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: ReducingStep) +: steps =>
        val typedTraversal = step match {
          case step: ReducingBarrierStep =>
            step match {
              case step: Mean =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et) //int to double?
              case step: Sum =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case _ => throw new Exception("unexpected")
            }
          case step: Head =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Last =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Min =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Max =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: FilterStep) +: steps =>
        val typedTraversal = step match {
          case step: And =>
            val typedStep = And(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: Coin =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Is =>
            new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
          case step: Not =>
            val typedStep = Not(step.traversal.retype(traversal.et, traversal.et))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: Or =>
            val typedStep = Or(step.traversals.map(t => t.retype(traversal.et, traversal.et)))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: Where =>
            val typedStep = Where(step.traversal.retype(traversal.et, traversal.et))
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
          case step: GlobalFilterStep =>
            step match {
              case step: Dedup =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
            }
          case step: ClipStep =>
            step match {
              case step: Limit =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case step: Range =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case step: Tail =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
              case step: Skip =>
                new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
            }
          case step: HasStep =>
            step match {
              case step: HasLabel =>
                new Traversal(step :: traversal.steps)(traversal.st,
                                                       step.label.reduceOption(_ + _).getOrElse(ClassType.stubNothing))
              case _ => new Traversal(step :: traversal.steps)(traversal.st, traversal.et)
            }
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: BarrierStep) +: steps =>
        val typedTraversal = step match {
          case step: Count =>
            new Traversal(step :: traversal.steps)(traversal.st, `@long`)
          case step: GroupingBarrierStep =>
            step match {
              case step: Group[_, _, _, _] =>
                val typedStep = Group(
                  step.by.retype(traversal.et, traversal.et),
                  step.value.retype(traversal.et, traversal.et)
                )
                new Traversal(typedStep :: traversal.steps)(
                  traversal.st,
                  TupleType(
                    List(
                      Some(typedStep.by.enclosedEndType),
                      Some((lspace.g
                        .out()
                        .untyped /*hack/manipulation because mapValues operates on a collection of traversers and typing assumes a singlepoint origin*/ ++ typedStep.value.untyped).toTyped
                        .retype(traversal.et, traversal.et)
                        .enclosedEndType)
                    ))
                )
            }
          case step: Order =>
            val typedStep = Order(step.by.retype(traversal.et, traversal.et), step.increasing)
            new Traversal(typedStep :: traversal.steps)(traversal.st, traversal.et)
        }
        stepsToTraversal(steps, typedTraversal)
      case (step: As[_, _]) +: steps =>
        val typedTraversal = new Traversal(As(step.label)(traversal.et) :: traversal.steps)(traversal.st, traversal.et)
        stepsToTraversal(steps, typedTraversal)
//      case _             =>
    }
  }
}

/**
  * TODO: try to convert End to shapeless.Coproduct
  * @param steps
  * @param st
  * @param et
  * @tparam ST
  * @tparam ET
  * @tparam Steps
  */
case class Traversal[+ST <: ClassType[Any], +ET <: ClassType[Any], +Steps <: HList] protected[lspace] (steps: Steps)(
    val st: ST,
    val et: ET) {

  lazy val stepsList: List[Step] =
    steps.runtimeList.asInstanceOf[List[Step]].reverse
//  lazy val stepsList: List[Step] = segmentList.flatMap(_.stepsList)

  def untyped: UntypedTraversal = UntypedTraversal(stepsList.toVector)

  def ++[ST0 <: ClassType[_], ET0 <: ClassType[_], Steps0 <: HList, Steps1 >: Steps <: HList, Out <: HList](
      traversal: Traversal[ST0, ET0, Steps0])(implicit //ev: ET <:< ST0,
                                              p1: Prepend.Aux[Steps0, Steps1, Out]): Traversal[ST, ET0, Out] =
    this.copy(p1(traversal.steps, steps))(st, traversal.et).retype(st, et).asInstanceOf[Traversal[ST, ET0, Out]]

  def retype(): Traversal[ClassType[Any], ClassType[Any], _ <: HList] =
    Traversal.stepsToTraversal(
      stepsList.toVector,
      Traversal(ClassType.stubAny, ClassType.stubAny).asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
  def retype(st: ClassType[Any], et: ClassType[Any]): Traversal[ClassType[Any], ClassType[Any], _ <: HList] =
    Traversal.stepsToTraversal(stepsList.toVector,
                               Traversal(st, et).asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
  def enclosedEndType: ClassType[Any] =
    EndMapper.tweakEnd(this.asInstanceOf[Traversal[ST, ET, HList]])

  override def equals(o: Any): Boolean = o match {
    case traversal: Traversal[ClassType[_], ClassType[_], HList] @unchecked =>
      stepsList == traversal.stepsList //&& st == traversal.st && et == traversal.et
  }

  lazy val toNode: Task[Node] = {
    for {
      node  <- DetachedGraph.nodes.create(Traversal.ontology)
      steps <- Task.gather(stepsList.map(_.toNode).toVector)
      e     <- if (steps.nonEmpty) node.addOut(keys.stepsNode, steps) else Task.unit
    } yield node
  }.memoizeOnSuccess

  def prettyPrint: String = {
//    segmentList.map(_.prettyPrint).mkString(".")
    stepsList.map(_.prettyPrint).mkString(".")
  }
}
