package lspace.librarian.process.traversal

import lspace.librarian.datatype._
import lspace.librarian.process.traversal.Traversal.{keys, ontology}
import lspace.librarian.process.traversal.helper.{ClassTypeable, Selector}
import lspace.librarian.process.traversal.step.Order.Orderable
import lspace.librarian.process.traversal.step.Select.Selection
import lspace.librarian.process.traversal.step._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem._
import lspace.librarian.structure.OntologyDef
import lspace.librarian.structure.PropertyDef
import lspace.librarian.structure._
import lspace.types.vector.Geometry
import lspace.util.types.DefaultsToAny
import monix.eval.Task
import shapeless.{::, <:!<, =:!=, HList, HNil, IsHCons1, LUBConstraint, Poly1, Witness, Id => _, Path => _, Select => _}
import shapeless.ops.hlist.{Collect, IsHCons, Mapper, Prepend, Reverse, ToList, ToTraversable, Unifier}

import scala.collection.immutable.ListSet

object Traversal
    extends OntologyDef(lspace.NS.vocab.Lspace.+("librarian/Traversal"), Set(), "Traversal", "A traversal .. ") {

  def toTraversal(node: Node)(target: Graph): Traversal[ClassType[Any], ClassType[Any], HList] = {
    val types = node.labels

    //    import shapeless.syntax.std.product._
    //    val steps0 = value0.out(Traversal.keys.stepNode).map(Step.toStep).toHList[Step :: HNil]
    val traversalSegments = node.out(Traversal.keys.segmentNode).take(1).flatten.foldLeft[HList](HNil) {
      case (hlist, node) =>
        Segment.toTraversalSegment(node) :: hlist
    }

    def findNearestParent(labels: List[ClassType[_]]): ClassType[_] = {
      if (labels.isEmpty) new DataType[Any] {
        override def iri: String = ""
      } else if (labels.toSet.size == 1) labels.head
      else
        labels.toSet match {
          case set if set.forall(_.`extends`(DataType.default.`@number`))   => DataType.default.`@number`
          case set if set.forall(_.`extends`(DataType.default.`@temporal`)) => DataType.default.`@temporal`
          case set if set.forall(_.`extends`(DataType.default.`@geo`))      => DataType.default.`@geo`
          case _ =>
            new DataType[Any] {
              override def iri: String = ""
            }
        }
    }

    def stepsToContainerStructure(steps: List[Any]): ClassType[_] = {
      import scala.collection.immutable.::
      steps.collect {
        case step: OutMap   => step
        case step: OutEMap  => step
        case step: InMap    => step
        case step: InEMap   => step
        case step: Group[_] => step
        case step: Path     => step
        case step: Project  => step
        case step: HasLabel => step
        //case step: Union => //collect differentiating HasLabel steps
      } match {
        case Nil =>
          new DataType[Any] {
            override def iri: String = ""
          }
        case step :: steps =>
          step match {
            case step: HasLabel => findNearestParent(step.label.map(_.iri).flatMap(target.ns.classtypes.get(_)))
            case step: OutMap   => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
            case step: OutEMap  => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
            case step: InMap    => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
            case step: InEMap   => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
            case step: Group[_] =>
              MapType(List(stepsToContainerStructure(step.by.segmentList)), List(stepsToContainerStructure(steps)))
            case step: Path => ListType(List(stepsToContainerStructure(step.by.segmentList)))
            case step: Project =>
              step.by.size match {
                case 2 =>
                  Tuple2Type(List(stepsToContainerStructure(step.by(0).segmentList)),
                             List(stepsToContainerStructure(step.by(1).segmentList)))
                case 3 =>
                  Tuple3Type(
                    List(stepsToContainerStructure(step.by(0).segmentList)),
                    List(stepsToContainerStructure(step.by(1).segmentList)),
                    List(stepsToContainerStructure(step.by(2).segmentList))
                  )
                case 4 =>
                  Tuple4Type(
                    List(stepsToContainerStructure(step.by(0).segmentList)),
                    List(stepsToContainerStructure(step.by(1).segmentList)),
                    List(stepsToContainerStructure(step.by(2).segmentList)),
                    List(stepsToContainerStructure(step.by(3).segmentList))
                  )
              }
          }
      }
    }

    new Traversal[ClassType[Any], ClassType[Any], HList](traversalSegments)(
      target,
      new DataType[Any] {
        override def iri: String = ""
      },
      stepsToContainerStructure(
        traversalSegments.runtimeList
          .asInstanceOf[List[Segment[HList]]]
          .flatMap(_.stepsList))
    )
  }

  object keys {
    object segment
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/Traversal/segment",
          "segment",
          "A segment in a traversal",
          `@range` = () => VectorType(Segment.ontology :: Nil) :: Nil
        ) {}
    lazy val segmentNode: TypedProperty[Vector[Node]] = segment.property as VectorType(Segment.ontology :: Nil)
  }

  override lazy val properties: List[Property] = keys.segment :: Nil

  trait Properties {
    lazy val `ns.l-space.eu/librarian/Traversal/segment`: Property                    = keys.segment
    lazy val `ns.l-space.eu/librarian/Traversal/segment@Node`: TypedKey[Vector[Node]] = keys.segmentNode
  }

  implicit class TraversalMod[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with FilterStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments]
      with CommonStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments]
      with ClipStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments]
      with MoveStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments]
      with MoveMapStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et

    def G(
        graph: Graph*): Traversal[ST[Start], GraphType[Graph], Segment[step.G :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST[Start], GraphType[Graph], Segment[G :: HNil] :: Segment[Steps] :: Segments]((new Segment[G :: HNil](
        step.G(graph.toList) :: HNil)) :: _traversal.segments)(_traversal.target, st, GraphType.datatype)

    def N(): Traversal[ST[Start], NodeURLType[Node], Segment[step.N :: HNil] :: Segment[Steps] :: Segments] =
      add(step.N(), st, NodeURLType.datatype)

    def N(resource: Node, resources: Node*)
      : Traversal[ST[Start], NodeURLType[Node], Segment[step.N :: HNil] :: Segment[Steps] :: Segments] =
      add(step.N(resource :: resources.toList), st, NodeURLType.datatype)

    def E: Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], Segment[step.E :: HNil] :: Segment[Steps] :: Segments] = //: Traversal[ST[Start],ClassType[Edge[S, E]], step.E :: Steps] =
      E[Any, Any]()

    def E[S: DefaultsToAny, E: DefaultsToAny](resource: Edge[S, E]*)
      : Traversal[ST[Start], EdgeURLType[Edge[S, E]], Segment[step.E :: HNil] :: Segment[Steps] :: Segments] =
      add(step.E(resource.toList.asInstanceOf[List[Edge[Any, Any]]]), st, EdgeURLType.apply[Edge[S, E]])

    def V(): Traversal[ST[Start], DataType[Any], Segment[step.V :: HNil] :: Segment[Steps] :: Segments] =
      add(step.V(), st, DataType.datatype)

    def V[T, OutC, Out <: ClassType[OutC]](value: T, values: T*)(implicit cls: ClassTypeable.Aux[T, OutC, Out])
      : Traversal[ST[Start], Out, Segment[step.V :: HNil] :: Segment[Steps] :: Segments] =
      add(step.V(value :: values.toList), st, cls.ct)

    //    def R[T <: Resource[T]: DefaultsToAny]: Traversal[Start, T, step.R :: Steps] = R()
    //    def R[T <: Resource[T]: DefaultsToAny](value: T*): Traversal[Start, T, step.R :: Steps] =
    //      Traversal[Start, T, step.R :: Steps](step.R(value.toList.asInstanceOf[List[Resource[Any]]]) :: _traversal.steps)
  }

  implicit class WithEmptyTraversal[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with FilterStepsHelper[Start, ST, End, ET, HNil, HNil, HNil]
      with CommonStepsHelper[Start, ST, End, ET, HNil, HNil, HNil]
      with ClipStepsHelper[Start, ST, End, ET, HNil, HNil, HNil]
      with MoveStepsHelper[Start, ST, End, ET, HNil, HNil, HNil]
      with MoveMapStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et

    def G(graph: Graph*): Traversal[ST[Start], GraphType[Graph], Segment[G :: HNil] :: HNil] =
      Traversal[ST[Start], GraphType[Graph], Segment[G :: HNil] :: HNil]((new Segment[G :: HNil](
        step.G(graph.toList) :: HNil)) :: _traversal.segments)(_traversal.target, st, GraphType.datatype)

    def N(): Traversal[ST[Start], NodeURLType[Node], Segment[step.N :: HNil] :: HNil] =
      add(step.N(), st, NodeURLType.datatype)

    def N(resource: Node, resources: Node*): Traversal[ST[Start], NodeURLType[Node], Segment[step.N :: HNil] :: HNil] =
      add(step.N(resource :: resources.toList), st, NodeURLType.datatype)

    def E: Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], Segment[step.E :: HNil] :: HNil] = //: Traversal[ST[Start],ClassType[Edge[S, E]], step.E :: Steps] =
      E[Any, Any]()

    def E[S: DefaultsToAny, E: DefaultsToAny](
        resource: Edge[S, E]*): Traversal[ST[Start], EdgeURLType[Edge[S, E]], Segment[step.E :: HNil] :: HNil] =
      add(step.E(resource.toList.asInstanceOf[List[Edge[Any, Any]]]), st, EdgeURLType.apply[Edge[S, E]])

    def V(): Traversal[ST[Start], DataType[Any], Segment[step.V :: HNil] :: HNil] =
      add(step.V(), st, DataType.datatype)

    def V[T, OutC, Out <: ClassType[OutC]](value: T, values: T*)(
        implicit cls: ClassTypeable.Aux[T, OutC, Out]): Traversal[ST[Start], Out, Segment[step.V :: HNil] :: HNil] =
      add(step.V(value :: values.toList), st, cls.ct)

    //    def R[T <: Resource[T]: DefaultsToAny]: Traversal[Start, T, step.R :: Steps] = R()
    //    def R[T <: Resource[T]: DefaultsToAny](value: T*): Traversal[Start, T, step.R :: Steps] =
    //      Traversal[Start, T, step.R :: Steps](step.R(value.toList.asInstanceOf[List[Resource[Any]]]) :: _traversal.steps)
  }

  trait BaseMod[Start,
                ST[+Z] <: ClassType[Z],
                End,
                ET[+Z] <: ClassType[Z],
                Steps <: HList,
                Segments <: HList,
                Segments1 <: HList] {

    def target: Graph
    def st: ST[Start]
    def et: ET[End]

    protected def add[S <: Step](step: S)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST[Start], ET[End], Segment[S :: Steps] :: Segments]
    protected def add[S <: MoveStep](step: S)(
        implicit ev: S <:< MoveStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segments1]
    protected def add[S <: ResourceStep](step: S)(
        implicit ev: S <:< ResourceStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segments1]
    protected def add[S <: Step, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST, ET, Segment[S :: Steps] :: Segments]
    protected def add[S <: MoveStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< MoveStep): Traversal[ST, ET, Segment[S :: HNil] :: Segments1]
    protected def add[S <: ResourceStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: Segments1]

    def id = add(Id, st, DataType.default.`@long`)

    def iri =
      add(Out(Set(Property.default.`@id`)), st, ClassType.default[Any]).hasLabel(DataType.default.`@string`)
//        .add(HasLabel(List(DataType.default.`@string`)), st, DataType.default.`@string`)
  }

  trait TModHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z]]
      extends BaseMod[Start, ST, End, ET, HNil, HNil, HNil] {

    protected def _traversal: Traversal[ST[Start], ET[End], HNil]

    import shapeless.<:!<

    protected def add[S <: Step](step: S)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, _traversal.st, _traversal.et)
    protected def add[S <: MoveStep](step: S)(
        implicit ev: S <:< MoveStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, _traversal.st, _traversal.et)
    protected def add[S <: ResourceStep](step: S)(
        implicit ev: S <:< ResourceStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, _traversal.st, _traversal.et)
    protected def add[S <: Step, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: HNil] =
      Traversal[ST, ET, Segment[S :: HNil] :: HNil]((new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(
        _traversal.target,
        st,
        et)
    protected def add[S <: MoveStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< MoveStep): Traversal[ST, ET, Segment[S :: HNil] :: HNil] =
      Traversal[ST, ET, Segment[S :: HNil] :: HNil]((new Segment[S :: HNil](step :: HNil)) :: HNil)(_traversal.target,
                                                                                                    st,
                                                                                                    et)
    protected def add[S <: ResourceStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: HNil] =
      Traversal[ST, ET, Segment[S :: HNil] :: HNil]((new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(
        _traversal.target,
        st,
        et)
  }

  trait TMod[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    protected def _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments]

    import shapeless.<:!<

    protected def add[S <: Step](step: S)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST[Start], ET[End], Segment[S :: Steps] :: Segments] =
      Traversal[ST[Start], ET[End], Segment[S :: Steps] :: Segments](
        _traversal.segments.head
          .copy(step :: _traversal.segments.head.steps) :: _traversal.segments.tail)(_traversal.target,
                                                                                     _traversal.st,
                                                                                     _traversal.et)

    protected def add[S <: MoveStep](step: S)(
        implicit ev: S <:< MoveStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, _traversal.st, _traversal.et)

    protected def add[S <: ResourceStep](step: S)(implicit ev: S <:< ResourceStep)
      : Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, _traversal.st, _traversal.et)

    protected def add[S <: Step, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST, ET, Segment[S :: Steps] :: Segments] =
      Traversal[ST, ET, Segment[S :: Steps] :: Segments](
        _traversal.segments.head
          .copy(step :: _traversal.segments.head.steps) :: _traversal.segments.tail)(_traversal.target, st, et)

    protected def add[S <: MoveStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< MoveStep): Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, st, et)

    protected def add[S <: ResourceStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.target, st, et)
  }

  trait FilterStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    implicit private def labelToProperty[L: HasStep.PropertyLabel](label: L): Property =
      label match {
        case label: Property    => label
        case label: PropertyDef => label.property
        case label: String =>
          target.ns.properties
            .get(label)
            .getOrElse(Property(label)) //throw new Exception("unknown key"))
      }

    //    def has[L: (String |∨| Property)#λ](label: L): Traversal[Start, End, Has :: Steps] = {
    def has[L: HasStep.PropertyLabel](label: L): Traversal[ST[Start], ET[End], Segment[Has :: Steps] :: Segments] =
      add(Has(label))
//    def has[L: HasStep.PropertyLabel, T <: HList](label: L, values: T)(
//        implicit d: LUBConstraint[T, P[_]],
//        ev: ToList[T, P[_]]): Traversal[ST[Start], ET[End], Segment[Has :: Steps] :: Segments] =
//      add(Has(label, values.toList))
    def has[L: HasStep.PropertyLabel, T](
        label: L,
        value: P[T]): Traversal[ST[Start], ET[End], Segment[Has :: Steps] :: Segments] =
      add(Has(label, Some(value)))

    def hasNot[L: HasStep.PropertyLabel](
        label: L): Traversal[ST[Start], ET[End], Segment[HasNot :: Steps] :: Segments] = add(HasNot(label))
    def hasNot[L: HasStep.PropertyLabel, T](
        label: L,
        value: P[T]): Traversal[ST[Start], ET[End], Segment[HasNot :: Steps] :: Segments] = {
      val step = HasNot(label, Some(value))
      add(step)
    }

    def hasId(id: Long, ids: Long*): Traversal[ST[Start], ET[End], Segment[HasId :: Steps] :: Segments] =
      add(HasId(id :: ids.toList toSet))
    def hasId(ids: Set[Long]): Traversal[ST[Start], ET[End], Segment[HasId :: Steps] :: Segments] = add(HasId(ids))

    /**
      * has at least one of the provided iris
      *
      * @param uri
      * @param uris
      * @return
      */
    def hasIri(iri: String, uris: String*): Traversal[ST[Start], ET[End], Segment[HasIri :: Steps] :: Segments] =
      add(HasIri(iri :: uris.toList toSet))
    def hasIri(iris: Set[String]): Traversal[ST[Start], ET[End], Segment[HasIri :: Steps] :: Segments] =
      add(HasIri(iris))

    /**
      * has at least one of the provided labels
      *
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

    def hasLabel(label: Ontology): Traversal[ST[Start], NodeURLType[Node], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label :: Nil), st, NodeURLType.datatype)
    def hasLabel(
        label: Property): Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label :: Nil), st, EdgeURLType.datatype)
    def hasLabel[T <: DataType[_], End1, ET1 <: ClassType[_]](label: T)(implicit et: ClassTypeable.Aux[T, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label :: Nil), st, et.ct)
//    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[_]](label0: ET0)(
//        implicit et: ClassTypeable.Aux[ET0, End1, ET1])
//      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
//      add(HasLabel(label0 :: Nil), st, et.ct)
    def hasLabel[ET0 <: ClassType[_], End1, ET1 <: ClassType[_]](label0: ET0, label1: ET0)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label0 :: label1 :: Nil), st, et.ct)
    def hasLabel[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[_]](label0: ET0[T], label1: ET0[T], label2: ET0[T])(
        implicit et: ClassTypeable.Aux[ET0[T], End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label0 :: label1 :: label2 :: Nil), st, et.ct)
    def hasLabel[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[_]](
        label0: ET0[T],
        label1: ET0[T],
        label2: ET0[T],
        label3: ET0[T])(implicit et: ClassTypeable.Aux[ET0[T], End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label0 :: label1 :: label2 :: label3 :: Nil), st, et.ct)
    //    def hasLabel[T](implicit ev: T <:!< ClassType[_], label: _ <: ClassType[T]) = {
    //      Traversal(HasLabel(label :: Nil) :: _traversal.steps)(target, st, label)
    //    }
    def hasLabel[A](
        implicit cls: ClassTypeable[A]): Traversal[ST[Start], cls.CT, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(cls.ct :: Nil), st, cls.ct)
    //    def hasLabel[A,B](implicit clsA: ClassTypeable[A], clsB: ClassTypeable[B]) = {
    //      Traversal[ST[Start], cls.CT, HasLabel :: Steps](HasLabel(cls.ct :: Nil) :: _traversal.steps)(target, st, cls.ct)
    //    }

    def isNumber: Traversal[ST[Start], NumericType[AnyVal], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@int` :: DataType.default.`@double` :: DataType.default.`@long` :: Nil),
          st,
          DataType.default.`@number`)

    def isTemporal: Traversal[ST[Start], CalendarType[Any], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@datetime` :: DataType.default.`@datetime` :: DataType.default.`@time` :: Nil),
          st,
          DataType.default.`@temporal`)

    def isQuantity: Traversal[ST[Start], QuantityType[Any], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@quantity` :: Nil), st, DataType.default.`@quantity`)

    def isDuration: Traversal[ST[Start], DurationType, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@duration` :: Nil), st, DataType.default.`@duration`)

    def isGeo: Traversal[ST[Start], GeometricType[Geometry], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@geo` :: Nil), st, DataType.default.`@geo`)

    def isColor: Traversal[ST[Start], ColorType[Any], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@color` :: Nil), st, DataType.default.`@color`)

    def hasLabel(label0: String,
                 labels0: String*): Traversal[ST[Start], ET[End], Segment[HasLabel :: Steps] :: Segments] = {
      val labelKeys = (target.ns.classtypes.get(label0) ::
        labels0
        .map(label => target.ns.classtypes.get(label))
        .toList).flatten
      add(HasLabel(labelKeys))
    }

    def coin(p: Double): Traversal[ST[Start], ET[End], Segment[Coin :: Steps] :: Segments] = add(Coin(p))
  }

  trait MoveMapStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def outMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          ClassType.default[Any])
    def outMap(keys: List[Property]): Traversal[ST[Start], ClassType[Any], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap(keys.toSet), st, ClassType.default[Any])
    def outMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], ClassType[Any], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap((f :: ff.toList).map(_.apply(Property.default)).toSet), st, ClassType.default[Any])
    def outMap(key: Property*): Traversal[ST[Start], ClassType[Any], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap(key.toSet), st, ClassType.default[Any])

    def outEMap(key: String,
                keys: String*): Traversal[ST[Start], ClassType[Any], Segment[OutEMap :: HNil] :: Segments1] =
      add(OutEMap(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          EdgeURLType.apply[Edge[End, Any]])
    def outEMap(keys: List[Property]): Traversal[ST[Start], ClassType[Any], Segment[OutEMap :: HNil] :: Segments1] =
      add(OutEMap(keys.toSet), st, EdgeURLType.apply[Edge[End, Any]])
    def outEMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], ClassType[Any], Segment[OutEMap :: HNil] :: Segments1] =
      add(OutEMap((f :: ff.toList).map(_.apply(Property.default)).toSet), st, EdgeURLType.apply[Edge[End, Any]])
    def outEMap(key: Property*): Traversal[ST[Start], ClassType[Any], Segment[OutEMap :: HNil] :: Segments1] =
      add(OutEMap(key.toSet), st, EdgeURLType.apply[Edge[End, Any]])

    def inMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], Segment[InMap :: HNil] :: Segments1] =
      add(InMap(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          ClassType.default[Any])
    def inMap(keys: List[Property]): Traversal[ST[Start], ClassType[Any], Segment[InMap :: HNil] :: Segments1] =
      add(InMap(keys.toSet), st, ClassType.default[Any])
    def inMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], ClassType[Any], Segment[InMap :: HNil] :: Segments1] =
      add(InMap((f :: ff.toList).map(_.apply(Property.default)).toSet), st, ClassType.default[Any])
    def inMap(key: Property*): Traversal[ST[Start], ClassType[Any], Segment[InMap :: HNil] :: Segments1] =
      add(InMap(key.toSet), st, ClassType.default[Any])

    def inEMap(key: String, keys: String*): Traversal[ST[Start], ClassType[Any], Segment[InEMap :: HNil] :: Segments1] =
      add(InEMap(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          EdgeURLType.apply[Edge[Any, End]])
    def inEMap(keys: List[Property]): Traversal[ST[Start], ClassType[Any], Segment[InEMap :: HNil] :: Segments1] =
      add(InEMap(keys.toSet), st, EdgeURLType.apply[Edge[Any, End]])
    def inEMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], ClassType[Any], Segment[InEMap :: HNil] :: Segments1] =
      add(InEMap((f :: ff.toList).map(_.apply(Property.default)).toSet), st, EdgeURLType.apply[Edge[Any, End]])
    def inEMap(key: Property*): Traversal[ST[Start], ClassType[Any], Segment[InEMap :: HNil] :: Segments1] =
      add(InEMap(key.toSet), st, EdgeURLType.apply[Edge[Any, End]])
  }

  trait MoveStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def out(key: String, keys: String*) =
      add(Out(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          ClassType.default[Any])
    def out(keys: List[Property]) = add(Out(keys.toSet), st, ClassType.default[Any])
    def out(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(Out((f :: ff.toList).map(_.apply(Property.default)).toSet), st, ClassType.default[Any])
    def out(key: Property*) = add(Out(key.toSet), st, ClassType.default[Any])

    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V])(implicit et: ClassTypeable.Aux[V, End1, ET1]) =
      add(Out(Set(key.key)), st, ClassType.default[Any]).hasLabel[V](et)
    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V], p: P[V])(
        implicit et: ClassTypeable.Aux[V, End1, ET1]) =
      add(Out(Set(key.key)), st, ClassType.default[Any]).is(p)

    def outE(key: String, keys: String*) =
      add(OutE(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
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
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          ClassType.default[Any])
    def in(keys: List[Property]) = add(In(keys.toSet), st, ClassType.default[Any])
    def in(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(In((f :: ff.toList).map(_.apply(Property.default)).toSet), st, ClassType.default[Any])
    def in(key: Property*) = add(In(key.toSet), st, ClassType.default[Any])

    def inE(key: String, keys: String*) =
      add(InE(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          EdgeURLType.apply[Edge[Any, End]])
    def inE(keys: List[Property]) = add(InE(keys.toSet), st, EdgeURLType.apply[Edge[Any, End]])
    def inE(f: (Property.default.type => Property), ff: (Property.default.type => Property)*) =
      add(InE((f :: ff.toList).map(_.apply(Property.default)).toSet), st, EdgeURLType.apply[Edge[Any, End]])
    def inE(key: Property*) = add(InE(key.toSet), st, EdgeURLType.apply[Edge[Any, End]])
  }

  trait CommonStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def drop(): Traversal[ST[Start], ET[End], Segment[Drop :: Steps] :: Segments] = add(Drop)

    def dedup(): Traversal[ST[Start], ET[End], Segment[Dedup :: Steps] :: Segments] = add(Dedup)

    def as[S <: String](name: () => S): Traversal[ST[Start], ET[End], Segment[As[End, S] :: Steps] :: Segments] =
      add(As[End, S](name()))

    def group[AZ <: ClassType[_]](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ, _ <: HList])
      : Traversal[ST[Start], ET[End], Segment[Group[AZ] :: Steps] :: Segments] =
      add(Group[AZ](by(Traversal[ET[End], ET[End]]()(target, et, et))))

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
    ): Traversal[ST[Start], Tuple2Type[A, B], Segment[Project :: Steps] :: Segments] /*: Traversal[ST[Start], TupleType[(A, B)], Segment[Project :: Steps] :: Segments]*/ = {
      val tby1 = by1(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby2 = by2(Traversal[ET[End], ET[End]]()(target, et, et))
      add(Project(List(tby1, tby2).asInstanceOf[List[Traversal[ET[End], ABZ, HList]]]),
          st,
          Tuple2Type(List(tby1.et), List(tby2.et)))
    }

    def project[A, AZ[+Z] <: ClassType[Z], B, BZ[+Z] <: ClassType[Z], C, CZ[+Z] <: ClassType[Z], ABCZ <: ClassType[_]](
        by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], _ <: HList],
        by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], _ <: HList],
        by3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CZ[C], _ <: HList])(
        implicit listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: CZ[C] :: HNil, List, ABCZ]
    ): Traversal[ST[Start], Tuple3Type[A, B, C], Segment[Project :: Steps] :: Segments] /*: Traversal[ST[Start], Tuple3Type[AZ[A], BZ[B], CZ[C]], Project :: Steps]*/ = {
      val tby1 = by1(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby2 = by2(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby3 = by3(Traversal[ET[End], ET[End]]()(target, et, et))
      add(Project(List(tby1, tby2, tby3).asInstanceOf[List[Traversal[ET[End], ABCZ, HList]]]),
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
    ): Traversal[ST[Start], Tuple4Type[A, B, C, D], Segment[Project :: Steps] :: Segments] /*: Traversal[ST[Start], Tuple4Type[AZ[A], BZ[B], CZ[C], DZ[D]], Project :: Steps]*/ = {
      val tby1 = by1(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby2 = by2(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby3 = by3(Traversal[ET[End], ET[End]]()(target, et, et))
      val tby4 = by4(Traversal[ET[End], ET[End]]()(target, et, et))
      add(
        Project(List(tby1, tby2, tby3, tby4).asInstanceOf[List[Traversal[ET[End], ABCDZ, HList]]]),
        st,
        Tuple4Type(List(tby1.et), List(tby2.et), List(tby3.et), List(tby4.et))
      )
    }

    def where(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])
      : Traversal[ST[Start], ET[End], Segment[Where :: Steps] :: Segments] =
      add(Where(traversal(Traversal[ET[End], ET[End]]()(target, et, et))))

    def and(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
            traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*)
      : Traversal[ST[Start], ET[End], Segment[And :: Steps] :: Segments] =
      add(
        And(
          traversal(Traversal[ET[End], ET[End]]()(target, et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]]()(target, et, et)))))

    def or(
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
        traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*
    ): Traversal[ST[Start], ET[End], Segment[Or :: Steps] :: Segments] =
      add(
        Or(
          traversal(Traversal[ET[End], ET[End]]()(target, et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]]()(target, et, et)))))

    def not(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])
      : Traversal[ST[Start], ET[End], Segment[Not :: Steps] :: Segments] =
      add(Not(traversal(Traversal[ET[End], ET[End]]()(target, et, et))))

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
        et0: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[Union[ET[End], ET0] :: Steps] :: Segments] = {
      val unionTraversal = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      add(
        Union[ET[End], ET0](
          unionTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]]()(target, et, et))
              .asInstanceOf[Traversal[ET[End], ET0, HList]])),
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
    def repeat[ET0 <: ClassType[_]](
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList],
        until: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList] = null,
        max: Int = 0,
        collect: Boolean = false): Traversal[ST[Start], ET0, Segment[Repeat[ET0] :: Steps] :: Segments] = {
      val t = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      add(Repeat(
            t,
            Option(until).map(_(Traversal[ET[End], ET[End]]()(target, et, et))),
            if (max == 0) None else Some(max),
            if (collect) Some(collect)
            else None
          ),
          st,
          t.et)
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
        et0: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[Coalesce[ET[End], ET0] :: Steps] :: Segments] = {
      val coalesceTraversal = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      add(
        Coalesce[ET[End], ET0](
          coalesceTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]]()(target, et, et))
              .asInstanceOf[Traversal[ET[End], ET0, HList]])),
        st,
        et0.ct
      )
    }

    def local[ET0 <: ClassType[_], Labels1 <: HList](
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList])
      : Traversal[ST[Start], ET0, Segment[Local :: Steps] :: Segments] = {
      val localTraversal = traversal(Traversal[ET[End], ET[End]]()(target, et, et))
      add(Local(localTraversal), st, localTraversal.et)
    }

    /**
      * TODO: result of path is a List[ET0]
      *
      * @param et0
      * @return
      */
    def path: Traversal[ST[Start], ClassType[Any], Segment[Path :: HNil] :: Segments1] =
      add(Path(Traversal[ET[End], ClassType[Any]]()(target, et, ClassType.default[Any])), st, ClassType.default[Any])

    def path[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1], Steps0 <: HList](
        traversal: Traversal[ET[End], ClassType[Any], HNil] => Traversal[ET[End], ET0, Steps0])(
        implicit
        et0: ClassTypeable.Aux[ET0, End1, ET1]
    ): Traversal[ST[Start], ClassType[Any], Segment[Path :: HNil] :: Segments1] = {
      val t = traversal(Traversal[ET[End], ClassType[Any]]()(target, et, ClassType.default[Any]))
      add(Path(t), st, et0.ct)
    }

    //    def is(value: End): Traversal[Start, Boolean, Is[End], Containers] =
    //      Traversal[Start, Boolean, Is[End], Containers](_traversal.step :+ Is(P.eq(value)))

    def is(predicate: P[End], predicates: P[_]*): Traversal[ST[Start], ET[End], Segment[Is :: Steps] :: Segments] =
      add(Is(predicate :: predicates.toList))
  }

  trait ClipStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def timeLimit(time: squants.time.Time): Traversal[ST[Start], ET[End], Segment[TimeLimit :: Steps] :: Segments] =
      add(TimeLimit(Some(time)))

    def noTimeLimit(): Traversal[ST[Start], ET[End], Segment[TimeLimit :: Steps] :: Segments] =
      add(TimeLimit())

    def range(low: Int, high: Int): Traversal[ST[Start], ET[End], Segment[Range :: Steps] :: Segments] =
      add(Range(low, high))

    def limit(max: Int): Traversal[ST[Start], ET[End], Segment[Limit :: Steps] :: Segments] = add(Limit(max))

    def tail(max: Int): Traversal[ST[Start], ET[End], Segment[Tail :: Steps] :: Segments] = add(Tail(max))

    def order[CT <: DataType[_]: Orderable](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList],
        increasing: Boolean = true): Traversal[ST[Start], ET[End], Segment[Order :: Steps] :: Segments] =
      add(Order(by(Traversal[ET[End], ET[End]]()(target, et, et)), increasing))

    def order[ET0 <: DataType[_]](increasing: Boolean)(implicit ctbl: ClassTypeable.Aux[ET[End], _, ET0])
      : Traversal[ST[Start], ET[End], Segment[Order :: Steps] :: Segments] =
      add(Order(Traversal()(target, et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))

    def count(): Traversal[ST[Start], LongType[Long], Segment[Count :: Steps] :: Segments] =
      add(Count, st, DataType.default.`@long`)
  }

  implicit class WithNodeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: NodeURLType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[Node], Segment[Steps] :: Segments])
      extends TMod[Start, ST, Node, ET, Steps, Segments]
      with NodeStepsHelper[Start, ST, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[Node]  = _traversal.et
  }
  implicit class WithNodeStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: NodeURLType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[Node], HNil])
      extends TModHNil[Start, ST, Node, ET]
      with NodeStepsHelper[Start, ST, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[Node]  = _traversal.et
  }
  trait NodeStepsHelper[
      Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, Node, ET, Steps, Segments, Segments1] {

    def label(key: String,
              keys: String*): Traversal[ST[Start], IriType[Ontology], Segment[Label :: HNil] :: Segments1] =
      add(Label((key :: keys.toList).map(key => target.ns.ontologies.get(key).getOrElse(Ontology(key))).toSet),
          st,
          DataType.default.`@class`)
    def label(
        keys: List[Ontology] = List()): Traversal[ST[Start], IriType[Ontology], Segment[Label :: HNil] :: Segments1] =
      add(Label(keys.toSet), st, DataType.default.`@class`)
    def label(key: Ontology*): Traversal[ST[Start], IriType[Ontology], Segment[Label :: HNil] :: Segments1] =
      add(Label(key.toSet), st, DataType.default.`@class`)
  }

  implicit class WithEdgeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: EdgeURLType[Z], Steps <: HList,
  Segments <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], Segment[Steps] :: Segments])
      extends TMod[Start, ST, Edge[In, Out], ET, Steps, Segments]
      with EdgeStepsHelper[Start, ST, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph         = _traversal.target
    def st: ST[Start]         = _traversal.st
    def et: ET[Edge[In, Out]] = _traversal.et
  }
  implicit class WithEdgeStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: EdgeURLType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], HNil])
      extends TModHNil[Start, ST, Edge[In, Out], ET]
      with EdgeStepsHelper[Start, ST, ET, HNil, HNil, HNil] {

    def target: Graph         = _traversal.target
    def st: ST[Start]         = _traversal.st
    def et: ET[Edge[In, Out]] = _traversal.et
  }
  trait EdgeStepsHelper[
      Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, Edge[In, Out], ET, Steps, Segments, Segments1] {

    def from[InC, InCT <: ClassType[InC]](implicit ct: ClassTypeable.Aux[In, InC, InCT])
      : Traversal[ST[Start], InCT, Segment[From :: HNil] :: Segments1] = {
      add(From: From, st, ct.ct)
    }

    def to[OutC, OutCT <: ClassType[OutC]](implicit ct: ClassTypeable.Aux[Out, OutC, OutCT])
      : Traversal[ST[Start], OutCT, Segment[To :: HNil] :: Segments1] = {
      add(To: To, st, ct.ct)
    }

    def label(key: String,
              keys: String*): Traversal[ST[Start], IriType[Property], Segment[Label :: HNil] :: Segments1] =
      add(Label(
            (key :: keys.toList)
              .map(
                key =>
                  target.ns.properties
                    .get(key)
                    .getOrElse(Property(key)))
              .toSet),
          st,
          DataType.default.`@property`)

    def label(
        keys: List[Property] = List()): Traversal[ST[Start], IriType[Property], Segment[Label :: HNil] :: Segments1] =
      add(Label(keys.toSet), st, DataType.default.`@property`)

    def label(key: Property*): Traversal[ST[Start], IriType[Property], Segment[Label :: HNil] :: Segments1] =
      add(Label(key.toSet), st, DataType.default.`@property`)
  }

  implicit class WithValueStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with ValueStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithValueStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with ValueStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  trait ValueStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def label[ET0 <: ET[End], End1, ET1 <: ClassType[End1]](keys: List[ET0] = List())(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[Label :: HNil] :: Segments1] =
      add(Label(keys.toSet), st, et.ct)

    def label[ET0 <: ET[End], End1, ET1 <: ClassType[End1]](key: ET0*)(implicit et: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[Label :: HNil] :: Segments1] =
      add(Label(key.toSet), st, et.ct)
  }

  implicit class WithNumericStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with NumericStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithNumericStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with NumericStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  trait NumericStepsHelper[Start,
                           ST[+Z] <: ClassType[Z],
                           End,
                           ET[+Z] <: NumericType[Z],
                           Steps <: HList,
                           Segments <: HList,
                           Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def sum(): Traversal[ST[Start], ET[End], Segment[Sum :: Steps] :: Segments] = add(Sum)
    def max(): Traversal[ST[Start], ET[End], Segment[Max :: Steps] :: Segments] = add(Max)
    def min(): Traversal[ST[Start], ET[End], Segment[Min :: Steps] :: Segments] = add(Min)
    def mean(): Traversal[ST[Start], DoubleType[Double], Segment[Mean :: Steps] :: Segments] =
      add(Mean, st, DataType.default.`@double`)

  }

  implicit class WithQuantityStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with QuantityStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithQuantityStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with QuantityStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  trait QuantityStepsHelper[Start,
                            ST[+Z] <: ClassType[Z],
                            End,
                            ET[+Z] <: QuantityType[Z],
                            Steps <: HList,
                            Segments <: HList,
                            Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def sum(): Traversal[ST[Start], ET[End], Segment[Sum :: Steps] :: Segments]   = add(Sum)
    def max(): Traversal[ST[Start], ET[End], Segment[Max :: Steps] :: Segments]   = add(Max)
    def min(): Traversal[ST[Start], ET[End], Segment[Min :: Steps] :: Segments]   = add(Min)
    def mean(): Traversal[ST[Start], ET[End], Segment[Mean :: Steps] :: Segments] = add(Mean)
  }

  implicit class WithTemporalStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with TemporalStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithTemporalStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with TemporalStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  trait TemporalStepsHelper[Start,
                            ST[+Z] <: ClassType[Z],
                            End,
                            ET[+Z] <: CalendarType[Z],
                            Steps <: HList,
                            Segments <: HList,
                            Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    //    def sum(): CalendarResultStep = CalendarResultStep()
    def max(): Traversal[ST[Start], ET[End], Segment[Max :: Steps] :: Segments] = add(Max)
    def min(): Traversal[ST[Start], ET[End], Segment[Min :: Steps] :: Segments] = add(Min)
  }

//  implicit class InstantSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[Instant], Segment[Steps] :: Segments])
//      extends TemporalSteps(_traversal)
//
//  implicit class LocalDateTimeSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[LocalDateTime], Segment[Steps] :: Segments])
//      extends TemporalSteps(_traversal)
//
//  implicit class LocalTimeSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[LocalTime], Segment[Steps] :: Segments])
//      extends TemporalSteps(_traversal)
//
//  implicit class LocalDateSteps[ET[+Z] <: CalendarType[Z]](
//      _traversal: Traversal[ST[Start], ET[LocalDate], Segment[Steps] :: Segments])
//      extends TemporalSteps(_traversal)

  implicit class WithGeoStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with GeoStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithGeoStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with GeoStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  trait GeoStepsHelper[Start,
                       ST[+Z] <: ClassType[Z],
                       End,
                       ET[+Z] <: GeometricType[Z],
                       Steps <: HList,
                       Segments <: HList,
                       Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def mean(): Traversal[ST[Start], ET[End], Segment[Mean :: Steps] :: Segments] = add(Mean)
  }

  object SegmentMapper extends Poly1 {
    implicit def getSteps[Steps <: HList] = at[Segment[Steps]](s => s.steps)
  }

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
//        protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])(
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
                                            Segments <: HList,
                                            AllSteps <: HList,
                                            Labels <: HList,
                                            SelectorOut <: Selector[_, HNil]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])(
      implicit
      //      val st: ST,
      //          val reverse: Reverse.Aux[Steps, RSteps],
      protected val flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segment[Steps] :: Segments, AllSteps],
      protected val f: Collect.Aux[AllSteps, LabelSteps.type, Labels],
      protected val lub: LUBConstraint[Labels, As[_, _]],
      protected val selector: SelectorSelecter.Aux[Labels, SelectorOut])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with AsAndSelectStepsHelper[Start,
                                  ST,
                                  End,
                                  ET,
                                  Steps,
                                  Segments,
                                  Segment[Steps] :: Segments,
                                  AllSteps,
                                  Labels,
                                  SelectorOut] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }

  implicit class WithAsAndSelectStepsHelperHNil[Start,
                                                ST[+Z] <: ClassType[Z],
                                                End,
                                                ET[+Z] <: ClassType[Z],
                                                Steps <: HList,
                                                Labels <: HList,
                                                SelectorOut <: Selector[_, HNil]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])(
      implicit
      //      val st: ST,
      //          val reverse: Reverse.Aux[Steps, RSteps],
      protected val flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, HNil, Steps],
      protected val f: Collect.Aux[Steps, LabelSteps.type, Labels],
      protected val lub: LUBConstraint[Labels, As[_, _]],
      protected val selector: SelectorSelecter.Aux[Labels, SelectorOut])
      extends TModHNil[Start, ST, End, ET]
      with AsAndSelectStepsHelper[Start, ST, End, ET, HNil, HNil, HNil, Steps, Labels, SelectorOut] {

    def target: Graph = _traversal.target
    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }

  trait AsAndSelectStepsHelper[Start,
                               ST[+Z] <: ClassType[Z],
                               End,
                               ET[+Z] <: ClassType[Z],
                               Steps <: HList,
                               Segments <: HList,
                               Segments1 <: HList,
                               AllSteps <: HList,
                               Labels <: HList,
                               SelectorOut <: Selector[_, HNil]]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    protected def flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segments1, AllSteps]
    protected def f: Collect.Aux[AllSteps, LabelSteps.type, Labels]
    protected def lub: LUBConstraint[Labels, As[_, _]]
    protected def selector: SelectorSelecter.Aux[Labels, SelectorOut]

    protected[this] def _traversal: Traversal[ST[Start], ET[End], Segments1]
    import shapeless.ops.hlist.{Selector => _, _}

    def select[RLabels <: HList, Types <: HList, End, End0, ET0 <: ClassType[_]](
        implicit
        reverse: Reverse.Aux[Labels, RLabels],
        ev: ToList[RLabels, As[_, _]],
        mapper: Mapper.Aux[LabelStepTypes.type, RLabels, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End0, ET0]): Traversal[ST[Start], ET0, Segment[Select[End] :: Steps] :: Segments] =
      add(Select[End](ev(reverse(f(flat(_traversal.segments)))).map(_.label.toString)), st, et.ct)

    def select[SelectedLabels <: HList, RSelectedLabels <: HList, End, End0, ET0 <: ClassType[End0]](
        label: SelectorOut => Selection[SelectedLabels, End])(implicit
                                                              //      lub: LUBConstraint[SelectedLabels, As[_]],
                                                              reverse: Reverse.Aux[SelectedLabels, RSelectedLabels],
                                                              ev: ToList[RSelectedLabels, As[_, _]],
                                                              et: ClassTypeable.Aux[End, End0, ET0]) =
      add(Select[End](ev(reverse(label(selector(f(flat(_traversal.segments)))).labels)).map(_.label.toString)),
          st,
          et.ct)

    implicitly[As[String, String] <:< As[_, String]]

    def select[A <: String, TypeA, OutA <: HList, End1, ET1 <: ClassType[_]](a: () => A)(
        implicit
        sel: CoFilter.Aux[Labels, As[_, A], OutA],
        mapper: Mapper.Aux[LabelStepTypes.type, OutA, TypeA :: HNil],
        et: ClassTypeable.Aux[TypeA, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[Select[OutA] :: Steps] :: Segments] =
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
        mapper: Mapper.Aux[LabelStepTypes.type, OutA :: OutB :: HNil, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End1, ET1]): Traversal[ST[Start], ET1, Segment[Select[End] :: Steps] :: Segments] =
      add(Select[End](List(a(), b())), st, et.ct)
  }

  def apply[ST0 <: ClassType[_], ET0 <: ClassType[_]]()(target: Graph, st: ST0, et: ET0): Traversal[ST0, ET0, HNil] =
    new Traversal[ST0, ET0, HNil](HNil)(target, st, et)

  def getCT[Start,
            ST[+Z] <: ClassType[Z],
            ET <: ClassType[_],
            Steps <: HList,
            Segments <: HList,
            RSteps <: HList,
            Containers <: HList,
            Out,
            CT <: ClassType[Out]](traversal: Traversal[ST[Start], ET, Segments])(
      implicit
      flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segments, Steps],
      reverse: Reverse.Aux[Steps, RSteps],
      f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
      lf: StructureCalculator.Aux[Containers, ET, Out, CT]): Option[CT] = {
    val ct =
      lf.convert(f(reverse(flat(traversal.segments))), traversal.et).headOption
    ct.foreach(MemGraphDefault.ns.classtypes.store)
    ct
  }

  implicit class WithTraversalStreamTyped[Start,
                                          ST[+Z] <: ClassType[Z],
                                          ET <: ClassType[_],
                                          Segments <: HList,
                                          Steps <: HList,
                                          RSteps <: HList,
                                          Containers <: HList,
                                          Out,
                                          CT <: ClassType[Out]](val traversal: Traversal[ST[Start], ET, Segments])(
      implicit
      val flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segments, Steps],
      val reverse: Reverse.Aux[Steps, RSteps],
      val f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
      val lf: StructureCalculator.Aux[Containers, ET, Out, CT])
      extends WithTraversalStream[Out] {

    lazy val ct =
      getCT[Start, ST, ET, Steps, Segments, RSteps, Containers, Out, CT](traversal)(flat, reverse, f, lf)

    protected[this] def stream: Stream[Out] =
      traversal.target.buildTraversersStream[ST[Start], ET, Segments, Out](traversal)
    protected[this] def astream: Task[Stream[Out]] =
      traversal.target.buildAsyncTraversersStream[ST[Start], ET, Segments, Out](traversal)
  }

  trait WithTraversalStream[Out] {
    protected[this] def stream: Stream[Out]
    protected[this] def astream: Task[Stream[Out]]

    def iterate(): Unit                  = stream.foreach(t => Unit)
    def head: Out                        = stream.head
    def headOption: Option[Out]          = stream.headOption
    def toList: List[Out]                = stream.toList
    def toListSet                        = stream.to[ListSet]
    def toSet: Set[Out]                  = stream.toSet
    def toStream: Stream[Out]            = stream
    def toAsyncStream: Task[Stream[Out]] = astream
    def toVector: Vector[Out]            = stream.toVector
    def next(n: Int)                     = stream.take(n)
  }

  def apply[Start: DefaultsToAny, End: DefaultsToAny](steps: Vector[Step])(target: Graph)(
      implicit cltblStart: ClassTypeable[Start],
      cltblEnd: ClassTypeable[End]): Traversal[cltblStart.CT, cltblEnd.CT, HList] = {
    import scala.collection.immutable.::
    val segments = steps.foldLeft(List[Segment[HList]]()) {
      case (head :: tail, step) =>
        step match {
          case step: FilterStep           => head.copy[HList](step :: head.steps) :: tail
          case step: RearrangeBarrierStep => head.copy[HList](step :: head.steps) :: tail
          case step                       => Segment().copy[HList](step :: HNil) :: head :: tail
        }
      case (Nil, step) =>
        Segment().copy[HList](step :: HNil) :: Nil
    }
    Traversal(segments.reverse.foldLeft[HList](HNil) {
      case (hl, segment) => segment :: hl
    })(target, cltblStart.ct, cltblEnd.ct)
  }
}

/**
  * TODO: try to convert End to shapeless.Coproduct
  * @param segments
  * @param target
  * @param st
  * @param et
  * @tparam ST
  * @tparam ET
  * @tparam Segments
  */
case class Traversal[+ST <: ClassType[_], +ET <: ClassType[_], Segments <: HList] protected[lspace] (segments: Segments)(
    val target: Graph,
    val st: ST,
    val et: ET) {

  lazy val segmentList: List[Segment[HList]] =
    segments.runtimeList.asInstanceOf[List[Segment[HList]]].reverse
  lazy val steps: List[Step] = segmentList.flatMap(_.stepsList)

  def untyped: UntypedTraversal = UntypedTraversal(segmentList.toVector)(target)
  def toUntypedStream: Stream[Any] =
    target.buildTraversersStream[ST, DataType[Any], HNil, Any](this.asInstanceOf[Traversal[ST, DataType[Any], HNil]])
  def toUntypedStreamTask: Task[Stream[Any]] =
    target.buildAsyncTraversersStream[ST, DataType[Any], HNil, Any](
      this.asInstanceOf[Traversal[ST, DataType[Any], HNil]])

  def withGraph(graph: Graph): Traversal[ST, ET, Segments] = {
    //    implicit val _target = target
    Traversal[ST, ET, Segments](segments)(target, st, et)
  }

  def ++[ST0 <: ClassType[_], ET0 <: ClassType[_], Segments0 <: HList, Out <: HList](
      traversal: Traversal[ST0, ET0, Segments0])(
      implicit p1: Prepend.Aux[Segments, Segments0, Out]): Traversal[ST, ET0, Out] =
    this.copy(p1.apply(segments, traversal.segments))(target, st, traversal.et)

  override def equals(o: Any): Boolean = o match {
    case traversal: Traversal[ClassType[_], ClassType[_], HList] => segmentList == traversal.segmentList
  }

  lazy val toNode: Node = {
    val node0 = DetachedGraph.nodes.create(ontology)
//    segmentList.map(_.toNode).foreach(node0.addOut(keys.segmentNode, _))
    node0.addOut(keys.segmentNode, segmentList.map(_.toNode).toVector)
    node0
  }

  def prettyPrint: String = {
    segmentList.map(_.prettyPrint).mkString(".")
  }
}
