package lspace.librarian.traversal

import lspace.datatype._
import lspace.librarian.logic.predicate.P
import lspace.librarian.task._
import lspace.librarian.traversal.Traversal.keys
import lspace.structure.util.{ClassTypeable, Selector}
import lspace.librarian.traversal.step.Order.Orderable
import lspace.librarian.traversal.step.Select.Selection
import lspace.librarian.traversal.step._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.types.vector.Geometry
import lspace.util.types.DefaultsToAny
import monix.eval.{Coeval, Task}
import shapeless.{::, <:!<, =:!=, HList, HNil, LUBConstraint, Poly1, Id => _, Path => _, Select => _}
import shapeless.ops.hlist.{Collect, Prepend, Reverse}

import scala.annotation.implicitNotFound

object Traversal
    extends OntologyDef(lspace.NS.vocab.Lspace.+("librarian/Traversal"), Set(), "Traversal", "A traversal .. ") {

  private val defaultdatatypestub = new DataType[Any] {
    override def iri: String = ""
  }
  def toTraversal(node: Node): Task[Traversal[ClassType[Any], ClassType[Any], HList]] = {
    val types = node.labels

    //    import shapeless.syntax.std.product._
    //    val steps0 = value0.out(Traversal.keys.stepNode).map(Step.toStep).toHList[Step :: HNil]
    for {
      traversalSegments <- Task
        .gather(node.out(Traversal.keys.segmentNode).take(1).flatten.map(Segment.toTraversalSegment))
        .map(_.foldLeft[HList](HNil) {
          case (hlist, step) => step :: hlist
        })
    } yield {

      def findNearestParent(labels: List[ClassType[_]], starttype: ClassType[Any]): ClassType[_] = {
        if (labels.isEmpty) starttype
        else if (labels.toSet.size == 1) labels.head
        else
          labels.toSet match {
            //          case set if set.forall(_.`extends`(DataType.default.`@int`))           => DataType.default.`@int`
            //          case set if set.forall(_.`extends`(DataType.default.`@double`))        => DataType.default.`@double`
            //          case set if set.forall(_.`extends`(DataType.default.`@long`))          => DataType.default.`@long`
            case set if set.forall(_.`extends`(DataType.default.`@number`)) => DataType.default.`@number`
            //          case set if set.forall(_.`extends`(DataType.default.`@datetime`))      => DataType.default.`@datetime`
            //          case set if set.forall(_.`extends`(DataType.default.`@localdatetime`)) => DataType.default.`@localdatetime`
            //          case set if set.forall(_.`extends`(DataType.default.`@date`))          => DataType.default.`@date`
            //          case set if set.forall(_.`extends`(DataType.default.`@time`))          => DataType.default.`@time`
            case set if set.forall(_.`extends`(DataType.default.`@temporal`)) => DataType.default.`@temporal`
            //          case set if set.forall(_.`extends`(DataType.default.`@geopoint`))      => DataType.default.`@geopoint`
            //          case set if set.forall(_.`extends`(DataType.default.`@geomultipoint`)) => DataType.default.`@geomultipoint`
            //          case set if set.forall(_.`extends`(DataType.default.`@geoline`))       => DataType.default.`@geoline`
            //          case set if set.forall(_.`extends`(DataType.default.`@geomultiline`))  => DataType.default.`@geomultiline`
            //          case set if set.forall(_.`extends`(DataType.default.`@geopolygon`))    => DataType.default.`@geopolygon`
            //          case set if set.forall(_.`extends`(DataType.default.`@geomultipolygon`)) =>
            //            DataType.default.`@geomultipolygon`
            //          case set if set.forall(_.`extends`(DataType.default.`@geomultigeo`)) => DataType.default.`@geomultigeo`
            case set if set.forall(_.`extends`(DataType.default.`@geo`)) => DataType.default.`@geo`
            case _ =>
              defaultdatatypestub
          }
      }

      def stepsToContainerStructure(steps: List[Any], starttype: ClassType[Any] = defaultdatatypestub): ClassType[_] = {
        import scala.collection.immutable.::
        steps.collect {
          case step: OutMap            => step
          case step: OutEMap           => step
          case step: InMap             => step
          case step: InEMap            => step
          case step: Group[_, _, _, _] => step
          case step: Path[_, _]        => step
          case step: Project[_]        => step
          case step: HasLabel          => step
          //case step: Union => //collect differentiating HasLabel steps
        } match {
          case Nil =>
            starttype
          case step :: steps =>
            step match {
              case step: HasLabel =>
                if (steps.nonEmpty)
                  stepsToContainerStructure(
                    steps,
                    findNearestParent(step.label.map(_.iri).flatMap(ClassType.classtypes.get), starttype))
                else findNearestParent(step.label.map(_.iri).flatMap(ClassType.classtypes.get), starttype)
              case step: OutMap  => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
              case step: OutEMap => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
              case step: InMap   => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
              case step: InEMap  => MapType(List(Property.ontology), List(stepsToContainerStructure(steps)))
              case step: Group[_, _, _, _] =>
                MapType(List(stepsToContainerStructure(step.by.segmentList)),
                        List(stepsToContainerStructure(steps), starttype))
              case step: Path[_, _] => ListType(List(stepsToContainerStructure(step.by.segmentList)))
              case step: Project[_] =>
                val traversals =
                  step.by.runtimeList.asInstanceOf[List[Traversal[ClassType[Any], ClassType[Any], HList]]]
                TupleType(traversals.map(_.segmentList).map(stepsToContainerStructure(_)).map(List(_)))
              //              traversals.size match {
              //                case 2 =>
              //                  Tuple2Type(List(stepsToContainerStructure(traversals(0).segmentList)),
              //                             List(stepsToContainerStructure(traversals(1).segmentList)))
              //                case 3 =>
              //                  Tuple3Type(
              //                    List(stepsToContainerStructure(traversals(0).segmentList)),
              //                    List(stepsToContainerStructure(traversals(1).segmentList)),
              //                    List(stepsToContainerStructure(traversals(2).segmentList))
              //                  )
              //                case 4 =>
              //                  Tuple4Type(
              //                    List(stepsToContainerStructure(traversals(0).segmentList)),
              //                    List(stepsToContainerStructure(traversals(1).segmentList)),
              //                    List(stepsToContainerStructure(traversals(2).segmentList)),
              //                    List(stepsToContainerStructure(traversals(3).segmentList))
              //                  )
              //              }
            }
        }
      }

      new Traversal[ClassType[Any], ClassType[Any], HList](traversalSegments)(
        defaultdatatypestub,
        stepsToContainerStructure(
          traversalSegments.runtimeList
            .asInstanceOf[List[Segment[HList]]]
            .flatMap(_.stepsList))
      )
    }
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

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et

    def G(
        graph: Graph*): Traversal[ST[Start], GraphType[Graph], Segment[step.G :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST[Start], GraphType[Graph], Segment[G :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[G :: HNil](step.G(graph.toList) :: HNil)) :: _traversal.segments)(st, GraphType.datatype)

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

    //FIX: within MoveStepsHelper the result type (ET1) freaks-out the compiler, it has trouble calculating it (IDEA shows correct types)
    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V])(implicit et: ClassTypeable.Aux[V, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Out :: HNil] :: Segment[Steps] :: Segments] =
      add(Out(Set(key.key)), st, ClassType.stubAny).hasLabel[V](et)

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

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et

    //G -> G[HList of Graph-like]
    def G(graph: Graph*): Traversal[ST[Start], GraphType[Graph], Segment[G :: HNil] :: HNil] =
      Traversal[ST[Start], GraphType[Graph], Segment[G :: HNil] :: HNil](
        (new Segment[G :: HNil](step.G(graph.toList) :: HNil)) :: _traversal.segments)(st, GraphType.datatype)

    def N(): Traversal[ST[Start], NodeURLType[Node], Segment[step.N :: HNil] :: HNil] =
      add(step.N(), st, Node.nodeUrl)

    def N(resource: Node, resources: Node*): Traversal[ST[Start], NodeURLType[Node], Segment[step.N :: HNil] :: HNil] =
      add(step.N(resource :: resources.toList), st, Node.nodeUrl)

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

    //FIX: within MoveStepsHelper the result type (ET1) freaks-out the compiler, it has trouble calculating it (IDEA shows correct types)
    def out[V, End1, ET1 <: ClassType[End1]](key: TypedProperty[V])(implicit et: ClassTypeable.Aux[V, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Out :: HNil] :: HNil] =
      add(Out(Set(key.key)), st, ClassType.stubAny).hasLabel[V](et)

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
      add(Out(Set(Property.default.`@id`)), st, ClassType.stubAny).hasLabel[String]
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
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.st, _traversal.et)
    protected def add[S <: MoveStep](step: S)(
        implicit ev: S <:< MoveStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.st, _traversal.et)
    protected def add[S <: ResourceStep](step: S)(
        implicit ev: S <:< ResourceStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: HNil](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.st, _traversal.et)
    protected def add[S <: Step, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: HNil] =
      Traversal[ST, ET, Segment[S :: HNil] :: HNil]((new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(st,
                                                                                                                   et)
    protected def add[S <: MoveStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< MoveStep): Traversal[ST, ET, Segment[S :: HNil] :: HNil] =
      Traversal[ST, ET, Segment[S :: HNil] :: HNil]((new Segment[S :: HNil](step :: HNil)) :: HNil)(st, et)
    protected def add[S <: ResourceStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: HNil] =
      Traversal[ST, ET, Segment[S :: HNil] :: HNil]((new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(st,
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
          .copy(step :: _traversal.segments.head.steps) :: _traversal.segments.tail)(_traversal.st, _traversal.et)

    protected def add[S <: MoveStep](step: S)(
        implicit ev: S <:< MoveStep): Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.st, _traversal.et)

    protected def add[S <: ResourceStep](step: S)(implicit ev: S <:< ResourceStep)
      : Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST[Start], ET[End], Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(_traversal.st, _traversal.et)

    protected def add[S <: Step, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:!< MoveStep,
        ev2: S <:!< ResourceStep): Traversal[ST, ET, Segment[S :: Steps] :: Segments] =
      Traversal[ST, ET, Segment[S :: Steps] :: Segments](
        _traversal.segments.head
          .copy(step :: _traversal.segments.head.steps) :: _traversal.segments.tail)(st, et)

    protected def add[S <: MoveStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< MoveStep): Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(st, et)

    protected def add[S <: ResourceStep, ST <: ClassType[_], ET <: ClassType[_]](step: S, st: ST, et: ET)(
        implicit ev: S <:< ResourceStep): Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments] =
      Traversal[ST, ET, Segment[S :: HNil] :: Segment[Steps] :: Segments](
        (new Segment[S :: HNil](step :: HNil)) :: _traversal.segments)(st, et)
  }

  trait FilterStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

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
    def hasLabels[ET0 <: ClassType[_], End1, ET1 <: ClassType[_]](label0: ET0, label1: ET0)(
        implicit et: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label0 :: label1 :: Nil), st, et.ct)
    def hasLabels[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[_]](
        label0: ET0[T],
        label1: ET0[T],
        label2: ET0[T])(implicit et: ClassTypeable.Aux[ET0[T], End1, ET1])
      : Traversal[ST[Start], ET1, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label0 :: label1 :: label2 :: Nil), st, et.ct)
    def hasLabels[T, ET0[+Z] <: ClassType[Z], End1, ET1 <: ClassType[_]](
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

    //TODO: create IsNode step
//    def isNode: Traversal[ST[Start], NodeURLType[Node], Segment[HasLabel :: Steps] :: Segments] =
//      add(HasLabel())
//
    //TODO: create IsEdge step
//    def isEdge: Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], Segment[HasLabel :: Steps] :: Segments] =
//      add(HasLabel())

    def isNumber: Traversal[ST[Start], NumericType[AnyVal], Segment[HasLabel :: Steps] :: Segments] =
//      add(HasLabel(DataType.default.`@int` :: DataType.default.`@double` :: DataType.default.`@long` :: Nil),
      add(HasLabel(DataType.default.`@number` :: Nil), st, DataType.default.`@number`)

    def isTemporal: Traversal[ST[Start], CalendarType[Any], Segment[HasLabel :: Steps] :: Segments] =
//      add(HasLabel(DataType.default.`@datetime` :: DataType.default.`@datetime` :: DataType.default.`@time` :: Nil),
      add(HasLabel(DataType.default.`@temporal` :: Nil), st, DataType.default.`@temporal`)

    def isQuantity: Traversal[ST[Start], QuantityType[Any], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@quantity` :: Nil), st, DataType.default.`@quantity`)

    def isDuration: Traversal[ST[Start], DurationType, Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@duration` :: Nil), st, DataType.default.`@duration`)

    def isGeo: Traversal[ST[Start], GeometricType[Geometry], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@geo` :: Nil), st, DataType.default.`@geo`)

    def isColor: Traversal[ST[Start], ColorType[Any], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(DataType.default.`@color` :: Nil), st, DataType.default.`@color`)

    def coin(p: Double): Traversal[ST[Start], ET[End], Segment[Coin :: Steps] :: Segments] = add(Coin(p))
//    def coin(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], DoubleType[Double], _ <: HList])
//    : Traversal[ST[Start], ET[End], Segment[Coin :: Steps] :: Segments] =
//      add(Coin(traversal(Traversal[ET[End], ET[End]](et, et))))

    def constant[T, T0, TT0 <: ClassType[_]](p: T)(implicit ct: ClassTypeable.Aux[T, T0, TT0])
      : Traversal[ST[Start], TT0, Segment[Constant[T, T0, TT0] :: HNil] :: Segments1] =
      add(Constant(p), st, ct.ct)
  }

  trait MoveMapStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def outMap(
        key: String,
        keys: String*): Traversal[ST[Start], MapType[Property, List[Any]], Segment[OutMap :: HNil] :: Segments1] =
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
        MapType(List(DataType.default.`@property`), List(ListType(Nil)))
      )
    def outMap(keys: List[Property])
      : Traversal[ST[Start], MapType[Property, List[Any]], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap(keys.toSet), st, MapType(List(DataType.default.`@property`), List(ListType(Nil))))
    def outMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Property, List[Any]], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
          st,
          MapType(List(DataType.default.`@property`), List(ListType(Nil))))
    def outMap(
        key: Property*): Traversal[ST[Start], MapType[Property, List[Any]], Segment[OutMap :: HNil] :: Segments1] =
      add(OutMap(key.toSet), st, MapType(List(DataType.default.`@property`), List(ListType(Nil))))

    def outEMap(key: String, keys: String*)
      : Traversal[ST[Start], MapType[Property, List[Edge[End, Any]]], Segment[OutEMap :: HNil] :: Segments1] =
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
        MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[End, Any]]))))
      )
    def outEMap(keys: List[Property])
      : Traversal[ST[Start], MapType[Property, List[Edge[End, Any]]], Segment[OutEMap :: HNil] :: Segments1] =
      add(OutEMap(keys.toSet),
          st,
          MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[End, Any]])))))
    def outEMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Property, List[Edge[End, Any]]], Segment[OutEMap :: HNil] :: Segments1] =
      add(
        OutEMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
        st,
        MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[End, Any]]))))
      )
    def outEMap(key: Property*)
      : Traversal[ST[Start], MapType[Property, List[Edge[End, Any]]], Segment[OutEMap :: HNil] :: Segments1] =
      add(OutEMap(key.toSet),
          st,
          MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[End, Any]])))))

    def inMap(key: String,
              keys: String*): Traversal[ST[Start], MapType[Property, List[Any]], Segment[InMap :: HNil] :: Segments1] =
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
        MapType(List(DataType.default.`@property`), List(ListType(Nil)))
      )
    def inMap(
        keys: List[Property]): Traversal[ST[Start], MapType[Property, List[Any]], Segment[InMap :: HNil] :: Segments1] =
      add(InMap(keys.toSet), st, MapType(List(DataType.default.`@property`), List(ListType(Nil))))
    def inMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Property, List[Any]], Segment[InMap :: HNil] :: Segments1] =
      add(InMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
          st,
          MapType(List(DataType.default.`@property`), List(ListType(Nil))))
    def inMap(key: Property*): Traversal[ST[Start], MapType[Property, List[Any]], Segment[InMap :: HNil] :: Segments1] =
      add(InMap(key.toSet), st, MapType(List(DataType.default.`@property`), List(ListType(Nil))))

    def inEMap(key: String, keys: String*)
      : Traversal[ST[Start], MapType[Property, List[Edge[Any, End]]], Segment[InEMap :: HNil] :: Segments1] =
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
        MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[Any, End]]))))
      )
    def inEMap(keys: List[Property])
      : Traversal[ST[Start], MapType[Property, List[Edge[Any, End]]], Segment[InEMap :: HNil] :: Segments1] =
      add(InEMap(keys.toSet),
          st,
          MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[Any, End]])))))
    def inEMap(f: (Property.default.type => Property), ff: (Property.default.type => Property)*)
      : Traversal[ST[Start], MapType[Property, List[Edge[Any, End]]], Segment[InEMap :: HNil] :: Segments1] =
      add(
        InEMap((f :: ff.toList).map(_.apply(Property.default)).toSet),
        st,
        MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[Any, End]]))))
      )
    def inEMap(key: Property*)
      : Traversal[ST[Start], MapType[Property, List[Edge[Any, End]]], Segment[InEMap :: HNil] :: Segments1] =
      add(InEMap(key.toSet),
          st,
          MapType(List(DataType.default.`@property`), List(ListType(List(EdgeURLType.apply[Edge[Any, End]])))))
  }

  trait MoveStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

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
      add(Out(Set(key.key)), st, ClassType.stubAny).hasLabel[V](et).is(p.asInstanceOf[P[End1]])

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
  }

  implicit class WithGroupStepHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], KOut,
  CK <: ClassType[_], KeySegments <: HList, Steps <: HList, Segments <: HList](
      protected[this] val _traversal: Traversal[ST[Start],
                                                TupleType[(KOut, List[End])],
                                                Segment[Group[CK, KeySegments, ET[End], HNil] :: Steps] :: Segments])
      extends TMod[Start, ST, (KOut, List[End]), TupleType, Group[CK, KeySegments, ET[End], HNil] :: Steps, Segments] {

    def st: ST[Start]                    = _traversal.st
    def et: TupleType[(KOut, List[End])] = _traversal.et
    def etV: ET[End]                     = _traversal.segments.head.steps.head.value.et

    def mapValues[CV <: ClassType[_],
                  ValueSegments <: HList,
                  VSteps <: HList,
                  VContainers <: HList,
                  VOut,
                  CVOut <: ClassType[_]](
        value: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CV, ValueSegments])(
        implicit
        flatV: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, ValueSegments, VSteps],
        fV: Collect.Aux[VSteps, ContainerSteps.type, VContainers],
        outV: OutTweaker.Aux[CV, VContainers, VOut, CVOut])
      : Traversal[ST[Start],
                  TupleType[(KOut, VOut)],
                  Segment[Group[CK, KeySegments, CV, ValueSegments] :: Steps] :: Segments] = {
      val step =
        Group[CK, KeySegments, CV, ValueSegments](_traversal.segments.head.steps.head.by, value(Traversal(etV, etV)))
      Traversal[ST[Start],
                TupleType[(KOut, VOut)],
                Segment[Group[CK, KeySegments, CV, ValueSegments] :: Steps] :: Segments](
        new Segment(step :: _traversal.segments.head.steps.tail) :: _traversal.segments.tail)(
        _traversal.st,
        TupleType[(KOut, VOut)](
          List(et.rangeTypes.head.asInstanceOf[List[ClassType[KOut]]], outV.tweak(step.value.et) :: Nil))
      )
    }

//      Traversal[ST[Start], ET[End], Segment[Group[AZ, KeySegments, CV[V], ValueSegments] :: Steps] :: Segments](
//        _traversal.segments.head
//          .copy(Group[AZ, KeySegments, AZv, ValueSegments](
//            _traversal.segments.head.steps.head.by,
//            value(Traversal[ET[End], ET[End]](et, et))) :: _traversal.segments.head.steps.tail) :: _traversal.segments.tail)(
//        _traversal.st,
//        _traversal.et)
  }

  object TMapper extends Poly1 {
    implicit def traversal[ST <: ClassType[_],
                           ET <: ClassType[_],
                           Segments <: HList,
                           Steps <: HList,
                           Containers <: HList,
                           Out,
                           COut <: ClassType[_]](
        implicit
        flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
        f: Collect.Aux[Steps, ContainerSteps.type, Containers],
        out: OutTweaker.Aux[ET, Containers, Out, COut]
    ): Case.Aux[Traversal[ST, ET, Segments], COut] = at[Traversal[ST, ET, Segments]](t => out.tweak(t.et))
  }
  object TOutMapper extends Poly1 {
    //    implicit def ct[T] = at[ClassType[T]](ct => 1.asInstanceOf[T])
    implicit def traversal[ST <: ClassType[_],
                           ET <: ClassType[_],
                           Segments <: HList,
                           Steps <: HList,
                           Containers <: HList,
                           Out,
                           COut <: ClassType[_]](
        implicit
        flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
        f: Collect.Aux[Steps, ContainerSteps.type, Containers],
        out: OutTweaker.Aux[ET, Containers, Out, COut]
    ): Case.Aux[Traversal[ST, ET, Segments], Out] = at[Traversal[ST, ET, Segments]](t => 1.asInstanceOf[out.Out])
  }

  implicit class WithProjectStepHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], PST <: ClassType[_],
  PET <: ClassType[_], PHSegments <: HList, PROJECTIONS <: HList, Steps <: HList, Segments <: HList](
      protected[this] val _traversal: Traversal[
        ST[Start],
        ET[End],
        Segment[Project[Traversal[PST, PET, PHSegments] :: PROJECTIONS] :: Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Project[Traversal[PST, PET, PHSegments] :: PROJECTIONS] :: Steps, Segments] {

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et

    def by[ALLPROJECTIONS <: HList, Out <: HList, EndH <: HList, End]()(
        implicit prepend: Prepend.Aux[Traversal[PST, PET, PHSegments] :: PROJECTIONS,
                                      Traversal[PST, PST, HNil] :: HNil,
                                      ALLPROJECTIONS],
        mapper: shapeless.ops.hlist.Mapper.Aux[TMapper.type, ALLPROJECTIONS, Out],
        mapper2: shapeless.ops.hlist.Mapper.Aux[TOutMapper.type, ALLPROJECTIONS, EndH], //only for type-calculation, never executed
        tupler: shapeless.ops.hlist.Tupler.Aux[EndH, End] //only for type-calculation, never executed
    ): Traversal[ST[Start], TupleType[End], Segment[Project[ALLPROJECTIONS] :: Steps] :: Segments] = {
      val step = Project[ALLPROJECTIONS](
        prepend(
          _traversal.segments.head.steps.head.by,
          Traversal(_traversal.segments.head.steps.head.by.head.st, _traversal.segments.head.steps.head.by.head.st) :: HNil
        ))
      Traversal[ST[Start], TupleType[End], Segment[Project[ALLPROJECTIONS] :: Steps] :: Segments](
        _traversal.segments.head
          .copy(step :: _traversal.segments.head.steps.tail) :: _traversal.segments.tail)(
        _traversal.st,
        TupleType[End](
          mapper(step.by).runtimeList.asInstanceOf[List[ClassType[Any]]].map(List(_)).map(_.filter(_.iri.nonEmpty))))
    }

    def by[P <: ClassType[_], PSegments <: HList, ALLPROJECTIONS <: HList, Out <: HList, EndH <: HList, End](
        value: Traversal[PST, PST, HNil] => Traversal[PST, P, PSegments])(
        implicit prepend: Prepend.Aux[Traversal[PST, PET, PHSegments] :: PROJECTIONS,
                                      Traversal[PST, P, PSegments] :: HNil,
                                      ALLPROJECTIONS],
        mapper: shapeless.ops.hlist.Mapper.Aux[TMapper.type, ALLPROJECTIONS, Out],
        mapper2: shapeless.ops.hlist.Mapper.Aux[TOutMapper.type, ALLPROJECTIONS, EndH], //only for type-calculation, never executed
        tupler: shapeless.ops.hlist.Tupler.Aux[EndH, End] //only for type-calculation, never executed
    ): Traversal[ST[Start], TupleType[End], Segment[Project[ALLPROJECTIONS] :: Steps] :: Segments] = {
      val step = Project[ALLPROJECTIONS](
        prepend(
          _traversal.segments.head.steps.head.by,
          value(Traversal(_traversal.segments.head.steps.head.by.head.st,
                          _traversal.segments.head.steps.head.by.head.st)) :: HNil
        ))
      Traversal[ST[Start], TupleType[End], Segment[Project[ALLPROJECTIONS] :: Steps] :: Segments](
        _traversal.segments.head
          .copy(step :: _traversal.segments.head.steps.tail) :: _traversal.segments.tail)(
        _traversal.st,
        TupleType[End](
          mapper(step.by).runtimeList.asInstanceOf[List[ClassType[Any]]].map(List(_)).map(_.filter(_.iri.nonEmpty))))
    }
  }

  trait CommonStepsHelper[
      Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, End, ET, Steps, Segments, Segments1] {

    def drop(): Traversal[ST[Start], ET[End], Segment[Drop :: Steps] :: Segments] = add(Drop)

    def dedup(): Traversal[ST[Start], ET[End], Segment[Dedup :: Steps] :: Segments] = add(Dedup)

    def as[S <: String](name: () => S): Traversal[ST[Start], ET[End], Segment[As[End, S] :: Steps] :: Segments] =
      add(As[End, S](name()))

    //TODO: add a 'byValue' traversal, so a traversal on the grouped result is contained within the step
    def group[CK <: ClassType[_],
              KeySegments <: HList,
              KSteps <: HList,
              KContainers <: HList,
              KOut,
              CKOut <: ClassType[_]](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CK, KeySegments])(
        implicit
        flatK: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, KeySegments, KSteps],
        fK: Collect.Aux[KSteps, ContainerSteps.type, KContainers],
        outK: OutTweaker.Aux[CK, KContainers, KOut, CKOut]
    ): Traversal[ST[Start],
                 TupleType[(KOut, List[End])],
                 Segment[Group[CK, KeySegments, ET[End], HNil] :: Steps] :: Segments] = {
      val step = Group[CK, KeySegments, ET[End], HNil](by(Traversal[ET[End], ET[End]](et, et)),
                                                       Traversal[ET[End], ET[End]](et, et))
      add(step,
          st,
          TupleType[(KOut, List[End])](
            List(outK.tweak(step.by.et) :: Nil filter (_.iri.nonEmpty),
                 ListType(step.value.et :: Nil filter (_.iri.nonEmpty)) :: Nil)))
    }

    //    def project[T, R](by: (Traversal[End, End, LastStep :: Steps] => Traversal[End, T, _, _ <: HList])*)
    //                     (traversals: List[Traversal[End, T, _, _ <: HList]] = by.toList.map(_(Traversal[End, End, LastStep]())))
    //                  (implicit f: FooTest.Aux[T, R], m: Monoid[R])
    //    : Traversal[Start, R, Project :: Steps] = {}

    def project(): Traversal[ST[Start],
                             TupleType[End],
                             Segment[Project[Traversal[ET[End], ET[End], HNil] :: HNil] :: Steps] :: Segments] =
      add(Project(Traversal(et, et) :: HNil), st, TupleType(List(List(et))))

    def project[CP <: ClassType[_],
                PSegments <: HList,
                PSteps <: HList,
                PContainers <: HList,
                POut,
                CPOut <: ClassType[_]](by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CP, PSegments])(
        implicit
        flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, PSegments, PSteps],
        f: Collect.Aux[PSteps, ContainerSteps.type, PContainers],
        out: OutTweaker.Aux[CP, PContainers, POut, CPOut]
    ): Traversal[ST[Start],
                 TupleType[POut],
                 Segment[Project[Traversal[ET[End], CP, PSegments] :: HNil] :: Steps] :: Segments] = {
      val tby1 = by1(Traversal(et, et))
      add(Project(tby1 :: HNil), st, TupleType(List(List(out.tweak(tby1.et)))))
    }

//    def project[A,
//                AZ[+Z] <: ClassType[Z],
//                B,
//                BZ[+Z] <: ClassType[Z],
//                ABZ <: ClassType[_],
//                Steps1 <: HList,
//                Steps2 <: HList](by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], Steps1],
//                                 by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], Steps2])(
//        implicit
//        listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: HNil, List, ABZ]
//    ): Traversal[
//      ST[Start],
//      ClassType[Nothing],
//      Segment[Project[Traversal[ET[End], AZ[A], Steps1] :: Traversal[ET[End], BZ[B], Steps2] :: HNil] :: Steps] :: Segments] /*: Traversal[ST[Start], TupleType[(A, B)], Segment[Project :: Steps] :: Segments]*/ = {
//      val tby1 = by1(Traversal[ET[End], ET[End]](et, et))
//      val tby2 = by2(Traversal[ET[End], ET[End]](et, et))
//      add(Project(tby1 :: tby2 :: HNil), st, ClassType.stubNothing)
//    }
//
//    def project[A,
//                AZ[+Z] <: ClassType[Z],
//                B,
//                BZ[+Z] <: ClassType[Z],
//                C,
//                CZ[+Z] <: ClassType[Z],
//                ABCZ <: ClassType[_],
//                Steps1 <: HList,
//                Steps2 <: HList,
//                Steps3 <: HList](by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], Steps1],
//                                 by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], Steps2],
//                                 by3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CZ[C], Steps3])(
//        implicit listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: CZ[C] :: HNil, List, ABCZ]
//    ): Traversal[ST[Start],
//                 ClassType[Nothing],
//                 Segment[Project[Traversal[ET[End], AZ[A], Steps1] :: Traversal[ET[End], BZ[B], Steps2] :: Traversal[
//                   ET[End],
//                   CZ[C],
//                   Steps3] :: HNil] :: Steps] :: Segments] /*: Traversal[ST[Start], Tuple3Type[AZ[A], BZ[B], CZ[C]], Project :: Steps]*/ = {
//      val tby1 = by1(Traversal[ET[End], ET[End]](et, et))
//      val tby2 = by2(Traversal[ET[End], ET[End]](et, et))
//      val tby3 = by3(Traversal[ET[End], ET[End]](et, et))
//      add(Project(tby1 :: tby2 :: tby3 :: HNil), st, ClassType.stubNothing)
//    }

//    def project[A,
//                AZ[+Z] <: ClassType[Z],
//                B,
//                BZ[+Z] <: ClassType[Z],
//                C,
//                CZ[+Z] <: ClassType[Z],
//                D,
//                DZ[+Z] <: ClassType[Z],
//                ABCDZ <: ClassType[_]](by1: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], AZ[A], _ <: HList],
//                                       by2: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], BZ[B], _ <: HList],
//                                       by3: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CZ[C], _ <: HList],
//                                       by4: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], DZ[D], _ <: HList])(
//        implicit listHelper: ToTraversable.Aux[AZ[A] :: BZ[B] :: CZ[C] :: DZ[D] :: HNil, List, ABCDZ]
//    ): Traversal[ST[Start], Tuple4Type[A, B, C, D], Segment[Project :: Steps] :: Segments] /*: Traversal[ST[Start], Tuple4Type[AZ[A], BZ[B], CZ[C], DZ[D]], Project :: Steps]*/ = {
//      val tby1 = by1(Traversal[ET[End], ET[End]](et, et))
//      val tby2 = by2(Traversal[ET[End], ET[End]](et, et))
//      val tby3 = by3(Traversal[ET[End], ET[End]](et, et))
//      val tby4 = by4(Traversal[ET[End], ET[End]](et, et))
//      add(
//        Project(List(tby1, tby2, tby3, tby4).asInstanceOf[List[Traversal[ET[End], ABCDZ, HList]]]),
//        st,
//        Tuple4Type(List(tby1.et), List(tby2.et), List(tby3.et), List(tby4.et))
//      )
//    }

    def where(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])
      : Traversal[ST[Start], ET[End], Segment[Where :: Steps] :: Segments] =
      add(Where(traversal(Traversal[ET[End], ET[End]](et, et))))

    def and(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
            traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*)
      : Traversal[ST[Start], ET[End], Segment[And :: Steps] :: Segments] =
      add(
        And(
          traversal(Traversal[ET[End], ET[End]](et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]](et, et)))))

    def or(
        traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
        traversals: (Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])*
    ): Traversal[ST[Start], ET[End], Segment[Or :: Steps] :: Segments] =
      add(
        Or(
          traversal(Traversal[ET[End], ET[End]](et, et)) ::
            traversals.toList
            .map(traversal => traversal(Traversal[ET[End], ET[End]](et, et)))))

    def not(traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList])
      : Traversal[ST[Start], ET[End], Segment[Not :: Steps] :: Segments] =
      add(Not(traversal(Traversal[ET[End], ET[End]](et, et))))

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
      val unionTraversal = traversal(Traversal[ET[End], ET[End]](et, et))
      add(
        Union[ET[End], ET0](
          unionTraversal ::
            traversals.toList.map(traversal =>
            traversal(Traversal[ET[End], ET[End]](et, et))
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
    def repeat[ET0 <: ClassType[_]](traversal: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, _ <: HList],
                                    max: Int = 0,
                                    collect: Boolean = false,
                                    noloop: Boolean = false)(
        implicit until: Traversal[ET0, ET0, HNil] => Traversal[ET0, _ <: ClassType[_], _ <: HList] = null
    ): Traversal[ST[Start], ET0, Segment[Repeat[ET0] :: Steps] :: Segments] = {
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

    def choose[ET0 <: ClassType[_],
               End1,
               ET1 <: ClassType[End1],
               Steps1 <: HList,
               Steps2 <: HList,
               Labels1 <: HList,
               Labels2 <: HList](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], _ <: ClassType[_], _ <: HList],
        right: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps1],
        left: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], ET0, Steps2])(
        implicit
        f1: Collect.Aux[Steps1, LabelSteps.type, Labels1],
        f2: Collect.Aux[Steps2, LabelSteps.type, Labels2],
        ev2: Labels1 =:= Labels2,
        et0: ClassTypeable.Aux[ET0, End1, ET1])
      : Traversal[ST[Start], ET1, Segment[Choose[ET[End], ET0] :: Steps] :: Segments] = {
      val byTraversal    = by(Traversal[ET[End], ET[End]](et, et))
      val rightTraversal = right(Traversal[ET[End], ET[End]](et, et))
      val leftTraversal  = left(Traversal[ET[End], ET[End]](et, et))
      add(
        Choose[ET[End], ET0](byTraversal, rightTraversal, leftTraversal),
        st,
        et0.ct
      )
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
      : Traversal[ST[Start], ET0, Segment[Local :: Steps] :: Segments] = {
      val localTraversal = traversal(Traversal[ET[End], ET[End]](et, et))
      add(Local(localTraversal), st, localTraversal.et)
    }

    /**
      * TODO: result of path is a List[ET0]
      *
      * @return
      */
    def path: Traversal[ST[Start], ClassType[Nothing], Segment[Path[ClassType[Any], HNil] :: HNil] :: Segments1] =
      add(Path(Traversal[ET[End], ClassType[Nothing]](et, ClassType.stubNothing)), st, ClassType.stubNothing)

    def path[ET0 <: ClassType[_], End1, ET1 <: ClassType[End1], Steps0 <: HList](
        traversal: Traversal[ET[End], ClassType[Any], HNil] => Traversal[ET[End], ET0, Steps0])(
        implicit
        et0: ClassTypeable.Aux[ET0, End1, ET1]
    ): Traversal[ST[Start], ClassType[Nothing], Segment[Path[ET0, Steps0] :: HNil] :: Segments1] = {
      val t = traversal(Traversal[ET[End], ClassType[Any]](et, ClassType.stubAny))
      add(Path(t), st, ClassType.stubNothing)
    }

//    def is[T, T0, TT0 <: ClassType[_]](value: T)(
//        implicit ev: T <:!< P[_],
//        ct: ClassTypeable.Aux[T, T0, TT0]): Traversal[ST[Start], ET[End], Segment[Is :: Steps] :: Segments] =
//      is(P.eqv(value)(ct))
    def is(predicate: P[End]): Traversal[ST[Start], ET[End], Segment[Is :: Steps] :: Segments] =
      add(Is(predicate))
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

    def head(): Traversal[ST[Start], ET[End], Segment[Head :: Steps] :: Segments]           = add(Head)
    def last(): Traversal[ST[Start], ET[End], Segment[Last :: Steps] :: Segments]           = add(Last)
    def limit(max: Int): Traversal[ST[Start], ET[End], Segment[Limit :: Steps] :: Segments] = add(Limit(max))
    def tail(max: Int): Traversal[ST[Start], ET[End], Segment[Tail :: Steps] :: Segments]   = add(Tail(max))

    def order[CT <: DataType[_]: Orderable](
        by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList],
        increasing: Boolean = true): Traversal[ST[Start], CT, Segment[Order :: Steps] :: Segments] = {
      val byTraversal = by(Traversal[ET[End], ET[End]](et, et))
      add(Order(byTraversal, increasing), st, byTraversal.et)
    }
    def max[CT <: DataType[_]: Orderable](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList])
      : Traversal[ST[Start], CT, Segment[Max :: Steps] :: Segments] = {
      val byTraversal = by(Traversal[ET[End], ET[End]](et, et))
      add(Max(byTraversal), st, byTraversal.et)
    }
    def min[CT <: DataType[_]: Orderable](by: Traversal[ET[End], ET[End], HNil] => Traversal[ET[End], CT, _ <: HList])
      : Traversal[ST[Start], CT, Segment[Min :: Steps] :: Segments] = {
      val byTraversal = by(Traversal[ET[End], ET[End]](et, et))
      add(Min(byTraversal), st, byTraversal.et)
    }

    def count(): Traversal[ST[Start], LongType[Long], Segment[Count :: Steps] :: Segments] =
      add(Count, st, DataType.default.`@long`)
  }

  implicit class WithNodeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[Node], Segment[Steps] :: Segments])
      extends TMod[Start, ST, Node, ET, Steps, Segments]
      with NodeStepsHelper[Start, ST, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def st: ST[Start] = _traversal.st
    def et: ET[Node]  = _traversal.et
  }
  implicit class WithNodeStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[Node], HNil])
      extends TModHNil[Start, ST, Node, ET]
      with NodeStepsHelper[Start, ST, ET, HNil, HNil, HNil] {

    def st: ST[Start] = _traversal.st
    def et: ET[Node]  = _traversal.et
  }
  trait NodeStepsHelper[
      Start, ST[+Z] <: ClassType[Z], ET[+Z] <: ClassType[Z], Steps <: HList, Segments <: HList, Segments1 <: HList]
      extends BaseMod[Start, ST, Node, ET, Steps, Segments, Segments1] {

    /**
      * this looks redundant w.r.t. the global FilterStepsHelper, but somehow a 'hasLabel' definition in NodeStepsHelper overwrites all other definitions... :S
      * @param label
      * @return
      */
    def hasLabel(label: Ontology): Traversal[ST[Start], NodeURLType[Node], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label :: Nil), st, NodeURLType.datatype)
    def hasLabel(label0: String,
                 labels0: String*): Traversal[ST[Start], ET[Node], Segment[HasLabel :: Steps] :: Segments] = {
      val labelKeys = (label0 :: labels0.toList).map(key => Ontology.ontologies.get(key).getOrElse(Ontology(key)))
      add(HasLabel(labelKeys))
    }

    def label(key: String,
              keys: String*): Traversal[ST[Start], IriType[Ontology], Segment[Label :: HNil] :: Segments1] =
      add(Label((key :: keys.toList).map(key => Ontology.ontologies.get(key).getOrElse(Ontology(key))).toSet),
          st,
          DataType.default.`@class`)
    def label(
        keys: List[Ontology] = List()): Traversal[ST[Start], IriType[Ontology], Segment[Label :: HNil] :: Segments1] =
      add(Label(keys.toSet), st, DataType.default.`@class`)
    def label(key: Ontology*): Traversal[ST[Start], IriType[Ontology], Segment[Label :: HNil] :: Segments1] =
      add(Label(key.toSet), st, DataType.default.`@class`)
  }

  implicit class WithEdgeStepsHelper[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: EdgeURLType[Z], Steps <: HList,
  Segments <: HList, In, Out](
      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], Segment[Steps] :: Segments])
      extends TMod[Start, ST, Edge[In, Out], ET, Steps, Segments]
      with EdgeStepsHelper[Start, ST, ET, Steps, Segments, Segment[Steps] :: Segments, In, Out] {

    def st: ST[Start]         = _traversal.st
    def et: ET[Edge[In, Out]] = _traversal.et
  }
  implicit class WithEdgeStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], ET[+Z] <: EdgeURLType[Z], In, Out](
      protected[this] val _traversal: Traversal[ST[Start], ET[Edge[In, Out]], HNil])
      extends TModHNil[Start, ST, Edge[In, Out], ET]
      with EdgeStepsHelper[Start, ST, ET, HNil, HNil, HNil, In, Out] {

    def st: ST[Start]         = _traversal.st
    def et: ET[Edge[In, Out]] = _traversal.et
  }
  trait EdgeStepsHelper[Start,
                        ST[+Z] <: ClassType[Z],
                        ET[+Z] <: ClassType[Z],
                        Steps <: HList,
                        Segments <: HList,
                        Segments1 <: HList,
                        In,
                        Out]
      extends BaseMod[Start, ST, Edge[In, Out], ET, Steps, Segments, Segments1] {

    def from[InC, InCT <: ClassType[InC]](implicit ct: ClassTypeable.Aux[In, InC, InCT])
      : Traversal[ST[Start], InCT, Segment[From :: HNil] :: Segments1] = {
      add(From: From, st, ct.ct)
    }

    def to[OutC, OutCT <: ClassType[OutC]](implicit ct: ClassTypeable.Aux[Out, OutC, OutCT])
      : Traversal[ST[Start], OutCT, Segment[To :: HNil] :: Segments1] = {
      add(To: To, st, ct.ct)
    }

    /**
      * this looks redundant w.r.t. the global FilterStepsHelper, but somehow a 'hasLabel' definition in EdgeStepsHelper overwrites all other definitions... :S
      * @param label
      * @return
      */
    def hasLabel(
        label: Property): Traversal[ST[Start], EdgeURLType[Edge[Any, Any]], Segment[HasLabel :: Steps] :: Segments] =
      add(HasLabel(label :: Nil), st, EdgeURLType.datatype)
    def hasLabel(label0: String,
                 labels0: String*): Traversal[ST[Start], ET[Edge[In, Out]], Segment[HasLabel :: Steps] :: Segments] = {
      val labelKeys = (label0 :: labels0.toList).map(key => Property.properties.get(key).getOrElse(Property(key)))
      add(HasLabel(labelKeys))
    }

    def label(key: String,
              keys: String*): Traversal[ST[Start], IriType[Property], Segment[Label :: HNil] :: Segments1] =
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

    def label(
        keys: List[Property] = List()): Traversal[ST[Start], IriType[Property], Segment[Label :: HNil] :: Segments1] =
      add(Label(keys.toSet), st, DataType.default.`@property`)

    def label(key: Property*): Traversal[ST[Start], IriType[Property], Segment[Label :: HNil] :: Segments1] =
      add(Label(key.toSet), st, DataType.default.`@property`)
  }

  implicit class WithValueStepsHelper[Start,
                                      ST[+Z] <: ClassType[Z],
                                      End,
                                      ET[+Z] <: DataType[Z],
                                      Steps <: HList,
                                      Segments <: HList](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])(
      implicit ev: ET[End] <:!< IriType[End])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with ValueStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithValueStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: DataType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])(implicit ev: ET[End] <:!< IriType[End])
      extends TModHNil[Start, ST, End, ET]
      with ValueStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

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

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithNumericStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: NumericType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with NumericStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

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
    def max(): Traversal[ST[Start], ET[End], Segment[Max :: Steps] :: Segments] =
      add(Max(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def min(): Traversal[ST[Start], ET[End], Segment[Min :: Steps] :: Segments] =
      add(Min(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def mean(): Traversal[ST[Start], DoubleType[Double], Segment[Mean :: Steps] :: Segments] =
      add(Mean, st, DataType.default.`@double`)
    def order[ET0 <: DataType[_]](
        increasing: Boolean): Traversal[ST[Start], ET[End], Segment[Order :: Steps] :: Segments] =
      add(Order(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))

  }

  implicit class WithQuantityStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with QuantityStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithQuantityStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: QuantityType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with QuantityStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

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

    def sum(): Traversal[ST[Start], ET[End], Segment[Sum :: Steps] :: Segments] = add(Sum)
    def max(): Traversal[ST[Start], ET[End], Segment[Max :: Steps] :: Segments] =
      add(Max(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def min(): Traversal[ST[Start], ET[End], Segment[Min :: Steps] :: Segments] =
      add(Min(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def mean(): Traversal[ST[Start], ET[End], Segment[Mean :: Steps] :: Segments] = add(Mean)
    def order[ET0 <: DataType[_]](
        increasing: Boolean): Traversal[ST[Start], ET[End], Segment[Order :: Steps] :: Segments] =
      add(Order(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))
  }

  implicit class WithTemporalStepsHelper[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z], Steps <: HList,
  Segments <: HList](protected[this] val _traversal: Traversal[ST[Start], ET[End], Segment[Steps] :: Segments])
      extends TMod[Start, ST, End, ET, Steps, Segments]
      with TemporalStepsHelper[Start, ST, End, ET, Steps, Segments, Segment[Steps] :: Segments] {

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithTemporalStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: CalendarType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with TemporalStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

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
    def max(): Traversal[ST[Start], ET[End], Segment[Max :: Steps] :: Segments] =
      add(Max(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def min(): Traversal[ST[Start], ET[End], Segment[Min :: Steps] :: Segments] =
      add(Min(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]])))
    def order[ET0 <: DataType[_]](
        increasing: Boolean): Traversal[ST[Start], ET[End], Segment[Order :: Steps] :: Segments] =
      add(Order(Traversal(et.asInstanceOf[DataType[_]], et.asInstanceOf[DataType[_]]), increasing))
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

    def st: ST[Start] = _traversal.st
    def et: ET[End]   = _traversal.et
  }
  implicit class WithGeoStepsHelperHNil[Start, ST[+Z] <: ClassType[Z], End, ET[+Z] <: GeometricType[Z]](
      protected[this] val _traversal: Traversal[ST[Start], ET[End], HNil])
      extends TModHNil[Start, ST, End, ET]
      with GeoStepsHelper[Start, ST, End, ET, HNil, HNil, HNil] {

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
        mapper: shapeless.ops.hlist.Mapper.Aux[LabelStepTypes.type, RLabels, Types],
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

    def select[A <: String, TypeA, OutA <: HList, End1, ET1 <: ClassType[_]](a: () => A)(
        implicit
        sel: CoFilter.Aux[Labels, As[_, A], OutA],
        mapper: shapeless.ops.hlist.Mapper.Aux[LabelStepTypes.type, OutA, TypeA :: HNil],
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
        mapper: shapeless.ops.hlist.Mapper.Aux[LabelStepTypes.type, OutA :: OutB :: HNil, Types],
        tupler: Tupler.Aux[Types, End],
        et: ClassTypeable.Aux[End, End1, ET1]): Traversal[ST[Start], ET1, Segment[Select[End] :: Steps] :: Segments] =
      add(Select[End](List(a(), b())), st, et.ct)
  }

  def apply[S: DefaultsToAny, E: DefaultsToAny]()(
      implicit cltblStart: ClassTypeable[S],
      cltblEnd: ClassTypeable[E]): Traversal[cltblStart.CT, cltblEnd.CT, HNil] =
    new Traversal(HNil: HNil)(cltblStart.ct, cltblEnd.ct)
  def apply[ST0 <: ClassType[_], ET0 <: ClassType[_]](st: ST0, et: ET0): Traversal[ST0, ET0, HNil] =
    new Traversal(HNil: HNil)(st, et)

//  def getCT[Start,
//            ST[+Z] <: ClassType[Z],
//            ET <: ClassType[_],
//            Steps <: HList,
//            Segments <: HList,
//            RSteps <: HList,
//            Containers <: HList,
//            Out,
//            CT <: ClassType[Out],
//            Out2](traversal: Traversal[ST[Start], ET, Segments])(
//      implicit
//      flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segments, Steps],
//      reverse: Reverse.Aux[Steps, RSteps],
//      f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
//      lf: StructureCalculator.Aux[Containers, ET, Out, CT],
//      tweaker: OutTweaker.Aux[ET, Out, Containers, Out2]): Option[ClassType[Out2]] = {
//    val ct =
//      lf.convert(f(reverse(flat(traversal.segments))), traversal.et)
//        .headOption
//        .map(et => tweaker.tweak(traversal.et, et))
//        .filter(_.iri.nonEmpty)
////    ct.foreach {
////      case ontology: Ontology =>
////        if (Ontology.ontologies.get(ontology.iri).isEmpty) Ontology.ontologies.cache(ontology)
////      case property: Property =>
////        if (Property.properties.get(property.iri).isEmpty) Property.properties.cache(property)
////      case datatype: DataType[_] =>
////        if (DataType.datatypes.get(datatype.iri).isEmpty) DataType.datatypes.cache(datatype)
////    }
//    ct
//  }

  implicit class WithMapTyped[Start,
                              ST[+Z] <: ClassType[Z],
                              K,
                              V,
                              Segments <: HList,
                              Steps <: HList,
                              RSteps <: HList,
                              Containers <: HList](val traversal: Traversal[ST[Start], MapType[K, V], Segments])(
      implicit
      val flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segments, Steps],
      val reverse: Reverse.Aux[Steps, RSteps],
      val f: Collect.Aux[RSteps, ContainerSteps.type, Containers]
  ) {

    @implicitNotFound("could not find a Guide or could not build the result type")
    def withGraph[F[_]](graph: Graph)(implicit
                                      guide: Guide[F],
                                      mapper: Mapper[F, Containers, Map[K, V]]): mapper.FT =
      mapper
        .apply(traversal.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]], graph)
        .asInstanceOf[mapper.FT]
    //    lazy val ct =
    //      getCT[Start, ST, ET, Steps, Segments, RSteps, Containers, Out, CT, Out2](traversal)(flat, reverse, f, lf, tweaker)
  }

  implicit class WithTraversalStreamTyped[Start,
                                          ST[+Z] <: ClassType[Z],
                                          End,
                                          ET[+Z] <: ClassType[Z],
                                          Segments <: HList,
                                          Steps <: HList,
                                          RSteps <: HList,
                                          Containers <: HList](val traversal: Traversal[ST[Start], ET[End], Segments])(
      implicit
      val flat: shapeless.ops.hlist.FlatMapper.Aux[SegmentMapper.type, Segments, Steps],
      val reverse: Reverse.Aux[Steps, RSteps],
      val f: Collect.Aux[RSteps, ContainerSteps.type, Containers]
//      val lf: StructureCalculator.Aux[Containers, ET, Out, CT],
//      val tweaker: OutTweaker.Aux[ET, Out, Containers, Out2]
  ) {

    @implicitNotFound("could not find a Guide or could not build the result type")
    def withGraph[ //iET >: ET <: ClassType[_],
        //    Steps <: HList,
        //    RSteps <: HList,
        //    Containers <: HList,
        F[_]](graph: Graph)(implicit
                            //      flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
                            //      reverse: Reverse.Aux[Steps, RSteps],
                            //      f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
                            //      lf: StructureCalculator.Aux[Containers, iET, Out, CT],
                            //      tweaker: OutTweaker.Aux[iET, Out, Containers, Out2],
                            guide: Guide[F],
                            mapper: Mapper[F, Containers, End]): mapper.FT =
      mapper
        .apply(traversal.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]], graph)
        .asInstanceOf[mapper.FT]
//    lazy val ct =
//      getCT[Start, ST, ET, Steps, Segments, RSteps, Containers, Out, CT, Out2](traversal)(flat, reverse, f, lf, tweaker)
  }

  def apply[Start: DefaultsToAny, End: DefaultsToAny](steps: Vector[Step])(
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
    })(cltblStart.ct, cltblEnd.ct)
  }
}

/**
  * TODO: try to convert End to shapeless.Coproduct
  * @param segments
  * @param st
  * @param et
  * @tparam ST
  * @tparam ET
  * @tparam Segments
  */
case class Traversal[+ST <: ClassType[_], +ET <: ClassType[_], Segments <: HList] protected[lspace] (segments: Segments)(
    val st: ST,
    val et: ET) {

  lazy val segmentList: List[Segment[HList]] =
    segments.runtimeList.asInstanceOf[List[Segment[HList]]].reverse
  lazy val steps: List[Step] = segmentList.flatMap(_.stepsList)

  def untyped: UntypedTraversal = UntypedTraversal(segmentList.toVector)

  def ++[ST0 <: ClassType[_], ET0 <: ClassType[_], Segments0 <: HList, Out <: HList](
      traversal: Traversal[ST0, ET0, Segments0])(
      implicit p1: Prepend.Aux[Segments, Segments0, Out]): Traversal[ST, ET0, Out] =
    this.copy(p1.apply(segments, traversal.segments))(st, traversal.et)

  override def equals(o: Any): Boolean = o match {
    case traversal: Traversal[ClassType[_], ClassType[_], HList] @unchecked => segmentList == traversal.segmentList
  }

  lazy val toNode: Task[Node] = {
    for {
      node     <- DetachedGraph.nodes.create(Traversal.ontology)
      segments <- Task.gather(segmentList.map(_.toNode).toVector)
      _        <- if (segments.nonEmpty) node.addOut(keys.segmentNode, segments) else Task.unit
    } yield node
  }.memoizeOnSuccess

  def prettyPrint: String = {
    segmentList.map(_.prettyPrint).mkString(".")
  }
}
