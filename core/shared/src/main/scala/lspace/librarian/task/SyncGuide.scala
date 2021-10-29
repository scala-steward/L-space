package lspace.librarian.task

import cats.implicits._
import lspace.librarian.logic.Assistent
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal._
import lspace.librarian.traversal.step._
import lspace.structure._
import monix.eval.Coeval
import shapeless.HList

import java.time._
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

object SyncGuide {
  def apply()(implicit _assistent: Assistent): SyncGuide = new SyncGuide {
    val assistent: Assistent = _assistent
  }
}
abstract class SyncGuide extends LocalGuide[LazyList] {

  type K[_] = Coeval[_]

  def emptyF[T]: LazyList[T]                    = LazyList.empty[T]
  def createF[T](t: T): LazyList[T]             = LazyList(t)
  def raiseError[T](ex: Exception): LazyList[T] = throw ex

  protected def head(f: LazyList[Librarian[Any]]): Coeval[Librarian[Any]] =
    Coeval(f.head)
  protected def headOption(f: LazyList[Librarian[Any]]): Coeval[Librarian[Any]] =
    Coeval(f.headOption.map(l => l.copy(Some(l.get).map(toValue))).getOrElse(createLibrarian(None)))
  protected def headOptionOption(f: LazyList[Librarian[Any]]): Coeval[Librarian[Any]] =
    Coeval(f.headOption.map(l => l.copy(toValue(l.get))).getOrElse(createLibrarian(None)))
  protected def toList(f: LazyList[Librarian[Any]]): Coeval[Librarian[Any]] =
    Coeval(f.toList.map(toValue)).map(createLibrarian(_))
  protected def toSet(f: LazyList[Librarian[Any]]): Coeval[Librarian[Any]] =
    Coeval(f.toList.map(toValue).toSet).map(createLibrarian(_))
  protected def toMap(f: LazyList[Librarian[(Any, Any)]]): Coeval[Librarian[Any]] =
    Coeval(f.toList.map(_.get).map { case (k, v) => toValue(k) -> toValue(v) }.toMap).map(createLibrarian(_))

  protected def takeByTimeSpan(f: LazyList[Librarian[Any]], timespan: FiniteDuration): LazyList[Librarian[Any]] = {
    val timeLimitStamp = Instant.ofEpochMilli(Instant.now().toEpochMilli + timespan.toMillis)
    f.takeWhile(_ => timeLimitStamp.isAfter(Instant.now()))
  }

  def resourceStep(step: ResourceStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: N =>
        if (
          step.nodes.forall {
            case node: Node =>
              node.graph == graph
            case v => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
          }
        ) {
          step.nodes match {
            case List() =>
              (obs: LazyList[Librarian[Any]]) =>
                obs
                  .flatMap { librarian =>
                    graph.nodeStore.cached
                      .all()
                      .asInstanceOf[LazyList[Node]]
                      .map(node =>
                        librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node))
                      )
                  }
            case list: List[Node] =>
              (obs: LazyList[Librarian[Any]]) =>
                obs.flatMap { librarian =>
                  list.map(node =>
                    librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node))
                  )
                }
          }
        } else { (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            graph.nodeStore.cached
              .all()
              .asInstanceOf[LazyList[Node]]
              .filter(step.nodes.contains)
              .map(node => librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node)))
          }
        }
      case step: E =>
        if (
          step.edges.forall {
            case edge: Edge[_, _] =>
              edge.graph == graph
            case v => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
          }
        ) {
          step.edges match {
            case List() =>
              (obs: LazyList[Librarian[Any]]) =>
                obs.flatMap { librarian =>
                  graph.edgeStore.cached
                    .all()
                    .asInstanceOf[LazyList[Edge[_, _]]]
                    .map(edge =>
                      librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge))
                    )
                }
            case list: List[Edge[_, _]] =>
              (obs: LazyList[Librarian[Any]]) =>
                obs.flatMap { librarian =>
                  list.map(edge =>
                    librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge))
                  )
                }
          }
        } else { (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            graph.edgeStore.cached
              .all()
              .asInstanceOf[LazyList[Edge[_, _]]]
              .filter(step.edges.contains)
              .map(edge => librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge)))
          }
        }
      case step: V =>
        if (
          step.values.forall {
            case edge: Value[_] =>
              edge.graph == graph
            case v => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
          }
        ) {
          step.values match {
            case List() =>
              (obs: LazyList[Librarian[Any]]) =>
                obs.flatMap { librarian =>
                  graph.valueStore.cached
                    .all()
                    .asInstanceOf[LazyList[Value[_]]]
                    .map(value =>
                      librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value))
                    )
                }
            case list: List[Value[_] @unchecked] =>
              (obs: LazyList[Librarian[Any]]) =>
                obs.flatMap { librarian =>
                  list.map(value =>
                    librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value))
                  )
                }
          }
        } else { (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            graph.valueStore.cached
              .all()
              .asInstanceOf[LazyList[Value[_]]]
              .filter(v => step.values.contains(v.value))
              .map(value => librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value)))
          }
        }
      //      case step: R =>
      case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
    }
    f.andThen(nextStep)
  }

  def traverseStep(step: TraverseStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)
    step match {
      case _: Id =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(obs.collect {
            case librarian if librarian.get.isInstanceOf[Resource[_]] =>
              librarian.copy(librarian.get.asInstanceOf[Resource[_]].id)
          })
      case _: From =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case e: Edge[_, _] =>
                  List(librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))
                case _ => List()
              }
            )
          )
      case _: To =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case e: Edge[_, _] =>
                  List(librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))
                case _ => List()
              }
            )
          )
      case step: Constant[_] =>
        (obs: LazyList[Librarian[Any]]) => nextStep(obs.map(librarian => librarian.copy(step.value)))
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
  }
  def moveStep(step: MoveStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)
    step match {
      case step: Out =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case r: Resource[_] =>
                  r.outE(step.label.toList: _*)
                    .map(e => librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))
                case _ => List()
              }
            )
          )
      case step: OutE =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case r: Resource[_] =>
                  r.outE(step.label.toList: _*)
                    .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
                case _ => List()
              }
            )
          )
      case step: In =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case r: Resource[_] =>
                  r.inE(step.label.toList: _*)
                    .map(e => librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))
                case _ => List()
              }
            )
          )
      case step: InE =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case r: Resource[_] =>
                  r.inE(step.label.toList: _*)
                    .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
                case _ => List()
              }
            )
          )
      case _: Label =>
        (obs: LazyList[Librarian[Any]]) =>
          nextStep(
            obs.flatMap(librarian =>
              librarian.get match {
                case r: Resource[_] =>
                  r.labels.map(label =>
                    librarian.copy(label, path = librarian.path.copy(librarian.path.resources :+ label))
                  )
                case _ => List()
              }
            )
          )
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
  }

  def filterStep(step: FilterStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: HasStep =>
        step match {
          case step: Has =>
            step.predicate.fold { (obs: LazyList[Librarian[Any]]) =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).nonEmpty
                  case _              => false
                }
              }
            } { (p: P[_]) =>
              val helper = assistent.pToHelper(p)
              (obs: LazyList[Librarian[Any]]) =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case _              => false
                  }
                }
            }
          case step: HasNot =>
            step.predicate.fold { (obs: LazyList[Librarian[Any]]) =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).isEmpty
                  case _              => true
                }
              }
            } { (p: P[_]) =>
              val helper = assistent.pToHelper(p)
              (obs: LazyList[Librarian[Any]]) =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => !r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case _              => true
                  }
                }
            }
          case step: HasId =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.filter(_.get match {
                case r: Resource[_] if step.ids.contains(r.id) => true
                case _                                         => false
              })
          case step: HasIri =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.filter(_.get match {
                case r: Resource[_] if step.iris.intersect(r.iris).nonEmpty => true
                case _                                                      => false
              })
          case step: HasLabel =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.filter(_.get match {
                case r: Resource[_] if step.label.exists(r.hasLabel(_).isDefined) => true
                case _                                                            => false
              })
          case step: HasValue =>
            val helper = assistent.pToHelper(step.predicate)
            (obs: LazyList[Librarian[Any]]) =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => helper.comparable(r.value) && helper.assert(r.value)
                  case v              => helper.assert(v)
                }
              }
          case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
        }
      case _: Dedup =>
//        import cats.Eq
//        implicit val eqFoo: Eq[Any] = Eq.fromUniversalEquals
        (obs: LazyList[Librarian[Any]]) =>
          val results = mutable.HashSet[Any]()
          obs.filter { l =>
            l.get match {
              case resource: Resource[Any] =>
                if (results.contains(resource.value)) false
                else {
                  results += resource.value
                  true
                }
              case v: Any =>
                if (results.contains(v)) false
                else {
                  results += v
                  true
                }
            }
          }
      case step: And =>
        val andObs = step.traversals.map(traversalToF)
        (obs: LazyList[Librarian[Any]]) =>
          obs.filter { librarian =>
            andObs.forall(_(librarian).nonEmpty)
          }
      case step: Or =>
        val orObs = step.traversals.map(traversalToF)
        (obs: LazyList[Librarian[Any]]) =>
          obs.filter { librarian =>
            orObs.exists(_(librarian).nonEmpty)
          }
      case step: Where =>
        val traveralObservable = traversalToF(step.traversal)
        (obs: LazyList[Librarian[Any]]) =>
          obs.filter { librarian =>
            traveralObservable(librarian).nonEmpty
          }
      case step: Not =>
        val traveralObservable = traversalToF(step.traversal)
        (obs: LazyList[Librarian[Any]]) =>
          obs.filter { librarian =>
            traveralObservable(librarian).isEmpty
          }
      case step: Coin =>
        (obs: LazyList[Librarian[Any]]) =>
          obs.filter { _ =>
            Math.random() < step.p // get next seeded random value
          }
      case step: Is =>
        val helper = assistent.pToHelper(step.predicate)
        (obs: LazyList[Librarian[Any]]) =>
          obs.filter { librarian =>
            librarian.get match {
              case r: Resource[_] => helper.assert(r.value)
              case v              => helper.assert(v)
            }
          }
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
    f.andThen(nextStep)
  }

  def reducingStep[T](step: ReducingStep)(implicit graph: Lspace): LazyList[Librarian[T]] => LazyList[Librarian[T]] = {

    val f = step match {
      case _: Head =>
        (obs: LazyList[Librarian[T]]) => obs.take(1)
      case _: Last =>
        (obs: LazyList[Librarian[T]]) => obs.lastOption.to(LazyList)
      case step: Min =>
        val byObservable = traversalToF(step.by)
        val byObsF = (obs: LazyList[Librarian[T]]) =>
          obs
            .flatMap { librarian =>
              byObservable(librarian)
                .map {
                  case l: Librarian[Any] => l.get
                  case v                 => v
                }
                .map {
                  case resource: Resource[Any] => resource.value
                  case v: Any                  => v
                }
                .map(librarian -> _)
            }
        import cats.implicits._
        (step.by.et.iri match {
          case lspace.NS.types.`@int` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[Int]))
          case lspace.NS.types.`@double` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[Double]))
          case lspace.NS.types.`@long` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[Long]))
          case lspace.NS.types.`@number` =>
            byObsF.andThen(_.minBy(_._2 match {
              case v: Int    => v.toDouble
              case v: Double => v
              case v: Long   => v.toDouble
              case v         => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
            }))
          case lspace.NS.types.`@datetime` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[Instant].toEpochMilli))
          case lspace.NS.types.`@localdatetime` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[LocalDateTime].toEpochSecond(ZoneOffset.UTC)))
          case lspace.NS.types.`@date` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[LocalDate].toEpochDay))
          case lspace.NS.types.`@time` =>
            byObsF.andThen(_.minBy(_._2.asInstanceOf[LocalTime].toNanoOfDay))
          case iri => throw new Exception(s"unexpected iri $iri")
        }).andThen(_._1).andThen(LazyList(_))
      case step: Max =>
        val byObservable = traversalToF(step.by)
        val byObsF = (obs: LazyList[Librarian[T]]) =>
          obs
            .flatMap { librarian =>
              byObservable(librarian)
                .map {
                  case l: Librarian[Any] => l.get
                  case v                 => v
                }
                .map {
                  case resource: Resource[Any] => resource.value
                  case v: Any                  => v
                }
                .map(librarian -> _)
            }
        (step.by.et.iri match {
          case lspace.NS.types.`@int` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[Int]))
          case lspace.NS.types.`@double` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[Double]))
          case lspace.NS.types.`@long` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[Long]))
          case lspace.NS.types.`@number` =>
            byObsF.andThen(_.maxBy(_._2 match {
              case v: Int    => v.toDouble
              case v: Double => v
              case v: Long   => v.toDouble
              case v         => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
            }))
          case lspace.NS.types.`@datetime` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[Instant].toEpochMilli))
          case lspace.NS.types.`@localdatetime` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[LocalDateTime].toEpochSecond(ZoneOffset.UTC)))
          case lspace.NS.types.`@date` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[LocalDate].toEpochDay))
          case lspace.NS.types.`@time` =>
            byObsF.andThen(_.maxBy(_._2.asInstanceOf[LocalTime].toNanoOfDay))
          case iri => throw new Exception(s"unexpected iri $iri")
        }).andThen(_._1).andThen(LazyList(_))
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
    f
  }
  def clipStep[T](step: ClipStep)(implicit graph: Lspace): LazyList[Librarian[T]] => LazyList[Librarian[T]] = {

    val f = step match {
      case step: Limit =>
        (obs: LazyList[Librarian[T]]) => obs.take(step.max)
      case step: Skip =>
        (obs: LazyList[Librarian[T]]) => obs.drop(step.n)
      case step: Range =>
        (obs: LazyList[Librarian[T]]) => obs.slice(step.low - 1, step.high)
      case step: Tail =>
        (obs: LazyList[Librarian[T]]) => obs.takeRight(step.max)
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
    f
  }

  def branchStep(step: BranchStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: Coalesce[_, _] =>
        val coalObs = step.traversals.map(traversalToF)
        (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            coalObs
              .to(LazyList)
              .map(t => Coeval.evalOnce(t(librarian)))
              .find(_.value().nonEmpty)
              .map(_.value())
              .toList
              .flatten
          }
      case step: Choose[_, _] =>
        val byObs    = traversalToF(step.by)
        val rightObs = traversalToF(step.right)
        val leftObs  = traversalToF(step.left)
        (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            byObs(librarian).nonEmpty match {
              case true =>
                rightObs(librarian)
              case false =>
                leftObs(librarian)
            }
          }
      case step: Local[_, _] =>
        val traveralObservable = traversalToF(step.traversal)
        (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            traveralObservable(librarian)
          }
      case step: Repeat[_] => // TODO: modify to take noloop-parameter into account
        val repeatObs = traversalToF(step.traversal)
        if (step.collect) {
          step.max match {
            case Some(max) =>
              step.until
                .filter(_.stepsList.nonEmpty)
//                .filter(_.segmentList.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(
                          obs.collect {
                            case librarian: Librarian[Any] if untilObs(librarian).isEmpty =>
                              librarian
                          },
                          maxMinOne
                        )
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), max))
                case None =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(
                          obs.collect { case librarian: Librarian[Any] =>
                            librarian
                          },
                          maxMinOne
                        )
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), max))
              }
            case None =>
              step.until
                .filter(_.stepsList.nonEmpty)
//                .filter(_.segmentList.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(
                          obs.collect {
                            case librarian: Librarian[Any] if untilObs(librarian).isEmpty =>
                              librarian
                          },
                          maxMinOne
                        )
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), 100)) // make configurable (fail-safe max-depth)
                case None =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(
                          obs.collect { case librarian: Librarian[Any] =>
                            librarian
                          },
                          maxMinOne
                        )
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), 100)) // make configurable (fail-safe max-depth)
              }
          }
        } else {
          step.max match {
            case Some(max) =>
              step.until
                .filter(_.stepsList.nonEmpty)
//                .filter(_.segmentList.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        val (finished, notFinished) = obs.partition {
                          case librarian: Librarian[Any] if untilObs(librarian).nonEmpty => true
                          case _                                                         => false
                        }
                        finished #::: repeat(notFinished.asInstanceOf[LazyList[Librarian[Any]]], maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), max))
                case None =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        repeat(
                          obs.collect { case librarian: Librarian[Any] =>
                            librarian
                          },
                          maxMinOne
                        )
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), max))
              }
            case None =>
              step.until
                .filter(_.stepsList.nonEmpty)
//                .filter(_.segmentList.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        val (finished, notFinished) = obs.partition {
                          case librarian: Librarian[Any] if untilObs(librarian).nonEmpty => true
                          case _                                                         => false
                        }
                        finished #::: repeat(notFinished.asInstanceOf[LazyList[Librarian[Any]]], maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), 100)) // make configurable (fail-safe max-depth)
                case None =>
                  (obs: LazyList[Librarian[Any]]) =>
                    def repeat(librarians: LazyList[Librarian[Any]], max: Int): LazyList[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        repeat(
                          obs.collect { case librarian: Librarian[Any] =>
                            librarian
                          },
                          maxMinOne
                        )
                      } else obs
                    }
                    obs.flatMap(l => repeat(LazyList(l), 100)) // make configurable (fail-safe max-depth)
              }
          }
        }
      case step: Union[_, _] =>
        val unionObs = step.traversals.map(traversalToF)
        (obs: LazyList[Librarian[Any]]) =>
          obs.flatMap { librarian =>
            unionObs.map(_(librarian)).reduce(_ ++ _)
          }
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
    f.asInstanceOf[LazyList[Librarian[Any]] => LazyList[Librarian[Any]]].andThen(nextStep)
  }

  def collectingBarrierStep(step: GroupingBarrierStep, steps: List[Step], isRootGroup: Boolean = false)(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {

    val nextStep = // buildNextStep(steps)
      steps match {
        case List(step: ClipStep) => clipStep[Any](step)
        case _ :: _ =>
          (_: LazyList[Librarian[Any]]) =>
            throw new Exception("GroupStep can only be followed by a ClipStep (currently)")
        case Nil =>
          (obs: LazyList[Librarian[Any]]) => obs
      }

    val f = step match {
      case step: Group[_, _, _, _] =>
        val byObservable    = traversalToF(step.by)
        val byMapper        = endMapper(step.by)
        val valueObservable = traversalsToF(step.value)
        val valueMapper = endMapper(
          (lspace.g
            .out()
            .untyped ++ step.value.untyped).toTyped
        )
//        val valueSteps = step.value.stepsList //.flatMap(_.stepsList)

        (obs: LazyList[Librarian[Any]]) =>
          obs
            .map { librarian =>
              byMapper(byObservable(librarian))
                .asInstanceOf[Coeval[Librarian[Any]]]
                .map(_.get)
                .map(librarian -> _)
                .value()
            }
            .groupBy(l => l._2)
            .to(LazyList)
            .map { group =>
              group._1 -> (if (step.value.stepsList.isEmpty) toList(valueObservable(group._2.map(_._1)))
                           else valueMapper(valueObservable(group._2.map(_._1))))
                .asInstanceOf[Coeval[Librarian[Any]]]
                .map(_.get)
                .value()
            }
            .map(createLibrarian(_))
            .asInstanceOf[LazyList[Librarian[Any]]]
      case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
    }
    f.andThen(nextStep)
  }

  def countStep(step: Count, steps: List[Step])(implicit
    graph: Graph
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case _: Count =>
        (obs: LazyList[Librarian[Any]]) => LazyList(createLibrarian(obs.size.toLong))
    }
    f.andThen(nextStep)
  }

  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case _: Mean =>
        (obs: LazyList[Librarian[Any]]) =>
          val temp = obs
            .map(_.get)
            .map {
              case l: Librarian[Any] => l.get
              case v                 => v
            }
            .map {
              case r: Resource[Any] => r.value
              case v                => v
            }
            .collect {
              case v: Int    => v
              case v: Double => v
              case v: Long   => v.toDouble
            }
          val sum   = temp.sum
          val count = temp.size

          LazyList(createLibrarian(if (count == 0) Double.NaN else sum / count))
      case _: Sum =>
        (obs: LazyList[Librarian[Any]]) =>
          LazyList(
            createLibrarian(
              obs
                .map(_.get)
//                .map {
//                  case l: Librarian[Any] => l.get
//                  case v                 => v
//                }
                .map {
                  case r: Resource[Any] => r.value
                  case v                => v
                }
                .collect {
                  case v: Int    => v
                  case v: Double => v
                  case v: Long   => v.toDouble
                }
                .sum
            )
          )
      case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
    }
    f.andThen(nextStep)
  }

//  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step])(
//      implicit graph: Lspace): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
//    val nextStep = buildNextStep(steps)
//
//    val f = step match {
//
//    }
//    f andThen nextStep
//  }

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: Order =>
        val byObservable = traversalToF(step.by)
        val byObsF = (obs: LazyList[Librarian[Any]]) =>
          obs
            .flatMap { librarian =>
              byObservable(librarian)
                .map {
                  case l: Librarian[Any] => l.get
                  case v                 => v
                }
                .map {
                  case resource: Resource[Any] => resource.value
                  case v: Any                  => v
                }
                .map(librarian -> _)
            }
        (step.by.et.iri match {
          case lspace.NS.types.`@string` =>
            val ordering = if (step.increasing) Ordering.String else Ordering.String.reverse
            byObsF.andThen(obs => obs.sortBy(_._2.asInstanceOf[String])(ordering))
          case lspace.NS.types.`@int` =>
            val ordering = if (step.increasing) Ordering.Int else Ordering.Int.reverse
            byObsF.andThen(obs => obs.sortBy(_._2.asInstanceOf[Int])(ordering))
          case lspace.NS.types.`@double` =>
            val ordering =
              if (step.increasing) lspace.datatype.util.Ordering.Double
              else lspace.datatype.util.Ordering.Double.reverse
            byObsF.andThen(obs => obs.sortBy(_._2.asInstanceOf[Double])(ordering))
          case lspace.NS.types.`@long` =>
            val ordering = if (step.increasing) Ordering.Long else Ordering.Long.reverse
            byObsF.andThen(obs => obs.sortBy(_._2.asInstanceOf[Long])(ordering))
          case lspace.NS.types.`@number` =>
            val ordering =
              if (step.increasing) lspace.datatype.util.Ordering.Double
              else lspace.datatype.util.Ordering.Double.reverse
            byObsF.andThen(obs =>
              obs.sortBy(_._2 match {
                case v: Int    => v.toDouble
                case v: Double => v
                case v: Long   => v.toDouble
                case v         => throw new Exception(s"unexpected type ${v.getClass.getSimpleName}")
              })(ordering)
            )
          case lspace.NS.types.`@datetime` =>
            if (step.increasing) {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: Instant), (_, v2: Instant)) =>
                    v1.isBefore(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            } else {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: Instant), (_, v2: Instant)) =>
                    v1.isAfter(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            }
          case lspace.NS.types.`@localdatetime` =>
            if (step.increasing) {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: LocalDateTime), (_, v2: LocalDateTime)) =>
                    v1.isBefore(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            } else {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: LocalDateTime), (_, v2: LocalDateTime)) =>
                    v1.isAfter(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            }
          case lspace.NS.types.`@date` =>
            if (step.increasing) {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: LocalDate), (_, v2: LocalDate)) =>
                    v1.isBefore(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            } else {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: LocalDate), (_, v2: LocalDate)) =>
                    v1.isAfter(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            }
          case lspace.NS.types.`@time` =>
            if (step.increasing) {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: LocalTime), (_, v2: LocalTime)) =>
                    v1.isBefore(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            } else {
              byObsF.andThen(obs =>
                obs.sortWith {
                  case ((_, v1: LocalTime), (_, v2: LocalTime)) =>
                    v1.isAfter(v2)
                  case _ => throw new Exception(s"Unsortable")
                }
              )
            }
          case iri => throw new Exception(s"unexpected iri $iri")
        }).andThen(_.map(_._1))
      case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
    }
    f.andThen(nextStep)
  }

  def projectionStep(step: ProjectionStep, steps: List[Step])(implicit
    graph: Graph
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: MapStep =>
        step match {
          case step: OutMap =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map(librarian =>
                librarian.copy(librarian.get match {
                  case r: Resource[_] =>
                    r.outEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        property -> nextStep(
                          edges
                            .to(LazyList)
                            .map(e =>
                              librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to))
                            )
                        ).toList
                      }
                  case _ => Map()
                })
              )
          case step: OutEMap =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map(librarian =>
                librarian.copy(librarian.get match {
                  case r: Resource[_] =>
                    r.outEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        property -> nextStep(
                          edges
                            .to(LazyList)
                            .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
                        ).toList
                      }
                  case _ => Map()
                })
              )
          case step: InMap =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map(librarian =>
                librarian.copy(librarian.get match {
                  case r: Resource[_] =>
                    r.inEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        property -> nextStep(
                          edges
                            .to(LazyList)
                            .map(e =>
                              librarian
                                .copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from))
                            )
                        ).toList
                      }
                  case _ => Map()
                })
              )
          case step: InEMap =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map(librarian =>
                librarian.copy(librarian.get match {
                  case r: Resource[_] =>
                    r.inEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        property -> nextStep(
                          edges
                            .to(LazyList)
                            .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
                        ).toList
                      }
                  case _ => Map()
                })
              )
          case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
        }
      case step: Project[_] =>
        projectStep(step, steps)
      case step: Path[_, _] =>
        val byObs = traversalToF(step.by)
        step.by.stepsList.lastOption match {
          case Some(Count) =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map { librarian =>
                librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).head)
              }
          case Some(_ @(_: Head | _: Min | _: Max | _: Mean)) =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map { librarian =>
                librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).headOption)
              }
          case Some(Last) =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map { librarian =>
                librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).lastOption)
              }
          case _ =>
            (obs: LazyList[Librarian[Any]]) =>
              obs.map { librarian =>
                librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).toList)
              }
        }
      case step: Select[_] =>
        selectStep(step, steps)
      case _ => throw new Exception(s"unexpected type ${step.getClass.getSimpleName}")
    }
    f.andThen { r =>
      r.map {
        case l: Librarian[Any] => l
        case other             => createLibrarian(other)
      }
    }.andThen(nextStep)
  }
  def projectStep[Traversals <: HList](step: Project[Traversals], steps: List[Step])(implicit
    graph: Lspace
  ): LazyList[Librarian[Any]] => LazyList[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val pObs = step.by.runtimeList.reverse.map {
      case traversal: Traversal[ClassType[Any], ClassType[Any], HList] @unchecked =>
        traversalToF(traversal) -> {
          (if (traversal.stepsList.isEmpty) { (observable: LazyList[Librarian[Any]]) =>
             head(observable)
           } else endMapper(traversal))
//          traversal.stepsList.lastOption
        }
      case t => throw new Exception(s"unexpected type ${t.getClass.getSimpleName}")
    }

    val f = (obs: LazyList[Librarian[Any]]) =>
      obs
        .map(librarian =>
          librarian -> pObs.map { case (pOb, endMap) =>
            endMap(pOb(librarian)).asInstanceOf[Coeval[Librarian[Any]]].map(_.get).value()
          }
        )
        .map { case (librarian, tuple) =>
          librarian.copy(tuple match {
            case List(v1)                     => v1
            case List(v1, v2)                 => (v1, v2)
            case List(v1, v2, v3)             => (v1, v2, v3)
            case List(v1, v2, v3, v4)         => (v1, v2, v3, v4)
            case List(v1, v2, v3, v4, v5)     => (v1, v2, v3, v4, v5)
            case List(v1, v2, v3, v4, v5, v6) => (v1, v2, v3, v4, v5, v6)
            case Nil                          => throw new Exception("cannot not project with zero projections")
            case _ => throw new Exception(s"cannot not project ${tuple.size} projections, max is 6 currently")
          })
        }

//    val f2 = step.by match {
//      case List(p1, p2) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        obs: LazyList[Librarian[Any]] =>
//          obs.map { librarian =>
//            librarian.copy((p1Obs(librarian).toList, p2Obs(librarian).toList))
//          }
//      case List(p1, p2, p3) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        val p3Obs = traversalToF(p3.segmentList)
//        obs: LazyList[Librarian[Any]] =>
//          obs.map { librarian =>
//            librarian.copy((p1Obs(librarian).toList, p2Obs(librarian).toList, p3Obs(librarian).toList))
//          }
//      case List(p1, p2, p3, p4) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        val p3Obs = traversalToF(p3.segmentList)
//        val p4Obs = traversalToF(p4.segmentList)
//        obs: LazyList[Librarian[Any]] =>
//          obs.map { librarian =>
//            librarian.copy(
//              (p1Obs(librarian).toList, p2Obs(librarian).toList, p3Obs(librarian).toList, p4Obs(librarian).toList))
//          }
//    }

    f.andThen(nextStep)
  }
}
