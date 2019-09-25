package lspace.librarian.task

import java.time._

import lspace.datatype.{CollectionType, ListSetType, ListType, MapType, OptionType, SetType, TupleType, VectorType}
import lspace.librarian.logic.Assistent
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal.step._
import lspace.librarian.traversal._
import lspace.structure._
import monix.eval.Task
import monix.reactive.Observable
import shapeless.HList

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

object AsyncGuide {
  def apply()(implicit _assistent: Assistent): AsyncGuide = new AsyncGuide {
    val assistent: Assistent = _assistent
  }
}
//TODO: keep track of the end-type (needed for efficient calculations and assertions)
abstract class AsyncGuide extends LocalGuide[Observable] {

  type K[_] = Task[_]

  def emptyF[T]: Observable[T]                    = Observable.empty[T]
  def createF[T](t: T): Observable[T]             = Observable(t)
  def raiseError[T](ex: Exception): Observable[T] = Observable.raiseError[T](ex)

  protected def head(f: Observable[Librarian[Any]]): Task[Librarian[Any]] =
    f.headL
  protected def headOption(f: Observable[Librarian[Any]]): Task[Librarian[Any]] =
    f.headOptionL.map(_.map(l => l.copy(Some(l.get).map(toValue))).getOrElse(createLibrarian(None)))
  protected def headOptionOption(f: Observable[Librarian[Any]]): Task[Librarian[Any]] =
    f.headOptionL.map(_.map(l => l.copy(toValue(l.get))).getOrElse(createLibrarian(None)))
  protected def toList(f: Observable[Librarian[Any]]): Task[Librarian[Any]] =
    f.toListL.map(toValue).map(createLibrarian(_))
  protected def toSet(f: Observable[Librarian[Any]]): Task[Librarian[Any]] =
    f.toListL.map(_.map(toValue).toSet).map(createLibrarian(_))
  protected def toMap(f: Observable[Librarian[(Any, Any)]]): Task[Librarian[Any]] =
    f.toListL.map(_.map(_.get).map { case (k, v) => toValue(k) -> toValue(v) }.toMap).map(createLibrarian(_))

  protected def takeByTimeSpan(f: Observable[Librarian[Any]], timespan: FiniteDuration): Observable[Librarian[Any]] =
    f.takeByTimespan(timespan)

  def resourceStep(step: ResourceStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: N =>
        if (step.nodes.forall {
              case node: Node => node.graph == this
            }) {
          step.nodes match {
            case List() =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  graph
                    .nodes()
                    .map(node =>
                      librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node)))
                }
            case list: List[Node] =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  Observable.fromIterable(list.map(node =>
                    librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node))))
                }
          }
        } else { obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            graph.nodeStore
              .hasIri(step.nodes.flatMap(_.iris).toSet)
              .asInstanceOf[Observable[Node]]
              .map(node => librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node)))
          }
        }
      case step: E =>
        if (step.edges.forall {
              case edge: Edge[_, _] => edge.graph == this
            }) {
          step.edges match {
            case List() =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  graph
                    .edges()
                    .map(edge =>
                      librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge)))
                }
            case list: List[Edge[_, _]] =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  Observable.fromIterable(list.map(edge =>
                    librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge))))
                }
          }
        } else { obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            graph.edgeStore
              .hasIri(step.edges.flatMap(_.iris).toSet)
              .asInstanceOf[Observable[Edge[_, _]]]
              .map(edge => librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge)))
          }
        }
      case step: V =>
        if (step.values.forall {
              case value: Value[_] => value.graph == this
            }) {
          step.values match {
            case List() =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  graph
                    .values()
                    .map(value =>
                      librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value)))
                }
            case list: List[Value[_] @unchecked] =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  Observable.fromIterable(list.map(value =>
                    librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value))))
                }
          }
        } else { obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            Observable
              .fromIterable(step.values)
              .flatMap(v =>
                graph.valueStore.byValue(v, ClassType.valueToOntologyResource(v)).asInstanceOf[Observable[Value[_]]])
              .asInstanceOf[Observable[Value[_]]]
              .map(value => librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value)))
          }
        }
      //      case step: R =>

    }
    f andThen nextStep
  }

  def traverseStep(step: TraverseStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {

    val nextStep = buildNextStep(steps)
    step match {
      case step: Id =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.collect {
            case librarian if librarian.get.isInstanceOf[Resource[_]] =>
              librarian.copy(librarian.get.asInstanceOf[Resource[_]].id)
          })
      case step: From =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case e: Edge[_, _] =>
                List(librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))
              case v => List()
            })))
      case step: To =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case e: Edge[_, _] =>
                List(librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))
              case v => List()
            })))
      case step: Constant[_] =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.map(librarian => librarian.copy(step.value)))
    }
  }

  def moveStep(step: MoveStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {

    val nextStep = buildNextStep(steps)
    step match {
      case step: Out =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case r: Resource[_] =>
                r.outE(step.label.toList: _*)
                  .map(e => librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))
              case v => List()
            })))
      case step: OutE =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case r: Resource[_] =>
                r.outE(step.label.toList: _*)
                  .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
              case v => List()
            })))
      case step: In =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case r: Resource[_] =>
                r.inE(step.label.toList: _*)
                  .map(e => librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))
              case v => List()
            })))
      case step: InE =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case r: Resource[_] =>
                r.inE(step.label.toList: _*)
                  .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
              case v => List()
            })))
      case step: Label =>
        obs: Observable[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            Observable.fromIterable(librarian.get match {
              case r: Resource[_] =>
                r.labels.map(label =>
                  librarian.copy(label, path = librarian.path.copy(librarian.path.resources :+ label)))
              case v => List()
            })))
    }
  }

  def filterStep(step: FilterStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: HasStep =>
        step match {
          case step: Has =>
            step.predicate.fold { obs: Observable[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).nonEmpty
                  case v              => false
                }
              }
            } { p: P[_] =>
              val helper = assistent.pToHelper(p)
              obs: Observable[Librarian[Any]] =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case v              => false
                  }
                }
            }
          case step: HasNot =>
            step.predicate.fold { obs: Observable[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).isEmpty
                  case v              => true
                }
              }
            } { p: P[_] =>
              val helper = assistent.pToHelper(p)
              obs: Observable[Librarian[Any]] =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => !r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case v              => true
                  }
                }
            }
          case step: HasId =>
            obs: Observable[Librarian[Any]] =>
              obs.filter(_.get match {
                case r: Resource[_] if step.ids.contains(r.id) => true
                case _                                         => false
              })
          case step: HasIri =>
            obs: Observable[Librarian[Any]] =>
              obs.filter(_.get match {
                case r: Resource[_] if step.iris intersect r.iris nonEmpty => true
                case _                                                     => false
              })
          case step: HasLabel =>
            obs: Observable[Librarian[Any]] =>
              obs.filter(_.get match {
                case r: Resource[_] if step.label.exists(r.hasLabel(_).isDefined) => true
                case _                                                            => false
              })
          case step: HasValue =>
            val helper = assistent.pToHelper(step.predicate)
            obs: Observable[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => helper.comparable(r.value) && helper.assert(r.value)
                  case v              => helper.assert(v)
                }
              }
        }
      case step: Dedup =>
        import cats.Eq
        implicit val eqFoo: Eq[Any] = Eq.fromUniversalEquals
        obs: Observable[Librarian[Any]] =>
          Observable
            .fromTask(
              //                obs.toListL.map(_.distinct) //BUG: does not work as expected... why?!
              obs.toListL.map { librarians =>
                val results = mutable.HashSet[Any]()
                librarians.filter { l =>
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
              }
            )
            .mergeMap(l => Observable.fromIterable(l))
      //              obj.groupBy(_.get) //BUG: does not filter like .distinct... why?!
      //              obs.groupBy(_.get match {
      //                case r: Resource[_] => r.value
      //                case v => v
      //              }).map(_.head).flatten
      case step: And =>
        val andObs = step.traversals.map(traversalToF)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            andObs.map(_(librarian)).map(_.nonEmpty).reduce(_ ++ _).forall(b => b).flatMap {
              case true  => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Or =>
        val orObs = step.traversals.map(traversalToF)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            orObs.map(_(librarian)).map(_.nonEmpty).reduce(_ ++ _).exists(b => b).flatMap {
              case true  => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Where =>
        val traveralObservable = traversalToF(step.traversal)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            traveralObservable(librarian).nonEmpty.flatMap {
              case true  => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Not =>
        val traveralObservable = traversalToF(step.traversal)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            traveralObservable(librarian).isEmpty.flatMap {
              case true  => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Coin =>
        obs: Observable[Librarian[Any]] =>
          obs.filter { librarian =>
            Math.random() < step.p //get next seeded random value
          }
      case step: Is =>
        val helper = assistent.pToHelper(step.predicate)
        obs: Observable[Librarian[Any]] =>
          obs.filter { librarian =>
            librarian.get match {
              case r: Resource[_] => helper.assert(r.value)
              case v              => helper.assert(v)
            }
          }
    }
    f andThen nextStep
  }

  //  def hasStep(step: HasStep, steps: List[Step])(
  //      obs: Observable[Librarian[Any]]): Observable[Librarian[Any]]
  def reducingStep[T](step: ReducingStep)(
      implicit graph: Graph): Observable[Librarian[T]] => Observable[Librarian[T]] = {

    val f = step match {
      case step: Head =>
        obs: Observable[Librarian[T]] =>
          obs.head
      case step: Last =>
        obs: Observable[Librarian[T]] =>
          obs.last
      case step: Min =>
        val byObservable = traversalToF(step.by)
        val byObsF = (obs: Observable[Librarian[T]]) =>
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
            byObsF andThen (_.minBy(_._2.asInstanceOf[Int]))
          case lspace.NS.types.`@double` =>
            byObsF andThen (_.minBy(_._2.asInstanceOf[Double]))
          case lspace.NS.types.`@long` =>
            byObsF andThen (_.minBy(_._2.asInstanceOf[Long]))
          case lspace.NS.types.`@number` =>
            byObsF andThen (_.minBy(_._2 match {
              case v: Int    => v.toDouble
              case v: Double => v
              case v: Long   => v.toDouble
            }))
          case lspace.NS.types.`@datetime` =>
            byObsF andThen (_.minBy(_._2.asInstanceOf[Instant].toEpochMilli))
          case lspace.NS.types.`@localdatetime` =>
            byObsF andThen (_.minBy(_._2.asInstanceOf[LocalDateTime].toEpochSecond(ZoneOffset.UTC)))
          case lspace.NS.types.`@date` =>
            byObsF andThen (_.minBy(_._2.asInstanceOf[LocalDate].toEpochDay))
          case lspace.NS.types.`@time` =>
            byObsF andThen (_.minBy(_._2.asInstanceOf[LocalTime].toNanoOfDay))
        }).andThen(_.map(_._1))
      case step: Max =>
        val byObservable = traversalToF(step.by)
        val byObsF = (obs: Observable[Librarian[T]]) =>
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
            byObsF andThen (_.maxBy(_._2.asInstanceOf[Int]))
          case lspace.NS.types.`@double` =>
            byObsF andThen (_.maxBy(_._2.asInstanceOf[Double]))
          case lspace.NS.types.`@long` =>
            byObsF andThen (_.maxBy(_._2.asInstanceOf[Long]))
          case lspace.NS.types.`@number` =>
            byObsF andThen (_.maxBy(_._2 match {
              case v: Int    => v.toDouble
              case v: Double => v
              case v: Long   => v.toDouble
            }))
          case lspace.NS.types.`@datetime` =>
            byObsF andThen (_.maxBy(_._2.asInstanceOf[Instant].toEpochMilli))
          case lspace.NS.types.`@localdatetime` =>
            byObsF andThen (_.maxBy(_._2.asInstanceOf[LocalDateTime].toEpochSecond(ZoneOffset.UTC)))
          case lspace.NS.types.`@date` =>
            byObsF andThen (_.maxBy(_._2.asInstanceOf[LocalDate].toEpochDay))
          case lspace.NS.types.`@time` =>
            byObsF andThen (_.maxBy(_._2.asInstanceOf[LocalTime].toNanoOfDay))
        }).andThen(_.map(_._1))
    }
    f
  }
  def clipStep[T](step: ClipStep)(implicit graph: Graph): Observable[Librarian[T]] => Observable[Librarian[T]] = {

    val f = step match {
      case step: Limit =>
        obs: Observable[Librarian[T]] =>
          obs.take(step.max)
      case step: Skip =>
        obs: Observable[Librarian[T]] =>
          obs.drop(step.n)
      case step: Range =>
        obs: Observable[Librarian[T]] =>
          obs.drop(step.low - 1).take(step.high + 1 - step.low)
      case step: Tail =>
        obs: Observable[Librarian[T]] =>
          obs.takeLast(step.max)
    }
    f
  }

  def branchStep(step: BranchStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: Coalesce[_, _] =>
        val coalObs = step.traversals.map(traversalToF)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            coalObs.foldLeft(Observable.empty[Any]) {
              case (obs, coalObs) =>
                obs.nonEmpty.flatMap {
                  case true  => obs
                  case false => coalObs(librarian)
                }
            }
          }
      case step: Choose[_, _] =>
        val byObs    = traversalToF(step.by)
        val rightObs = traversalToF(step.right)
        val leftObs  = traversalToF(step.left)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            byObs(librarian).nonEmpty.flatMap {
              case true =>
                rightObs(librarian)
              case false =>
                leftObs(librarian)
            }
          }
      case step: Local[_, _] =>
        val traveralObservable = traversalToF(step.traversal)
        obs: Observable[Librarian[Any]] =>
          obs.mapEval { librarian =>
            val result = traveralObservable(librarian)
            step.traversal.et match {
              case et: CollectionType[_] =>
                et match {
                  case et: MapType[_] =>
                    result.toListL
                      .map { l =>
                        l.asInstanceOf[List[Librarian[(Any, Any)]]].map(_.get).toMap
                      }
                      .map(librarian.copy(_))
                  case et: OptionType[_] => result.headOptionL
                  case et =>
                    result.toListL
                      .map { l =>
                        l.asInstanceOf[List[Librarian[Any]]].map(_.get)
                      }
                      .map(librarian.copy(_))
                }
              case et => result.headL
            }
          }

      case step: Repeat[_] => //TODO: modify to take noloop-parameter into account
        import scala.concurrent.duration._
        val repeatObs = traversalToF(step.traversal)
        lazy val noloop = if (step.noloop) (librarian: Librarian[Any]) => {
          librarian.get match {
            case r: Resource[_] => !librarian.path.resources.dropRight(1).contains(librarian.get)
            case _              => true
          }
        } else (librarian: Librarian[Any]) => true
        (if (step.collect) {
           step.max match {
             case Some(max) =>
               step.until
                 .filter(_.stepsList.nonEmpty)
//                 .filter(_.segmentList.head.stepsList.nonEmpty)
                 .map(traversalToF) match {
                 case Some(untilObs) =>
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable
                         .tailRecM(librarian -> max) {
                           case (librarian, max) =>
                             val r = repeatObs(librarian).filter(noloop(_))
                             r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                               untilObs(librarian).nonEmpty.flatMap {
                                 case true => Observable(Right(librarian))
                                 case false =>
                                   if (max > 0) Observable(Right(librarian)) ++ Observable(Left(librarian -> (max - 1)))
                                   else Observable(Right(librarian))
                               }
                             }
                         }
                     }
                 case None =>
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable.tailRecM(librarian -> max) {
                         case (librarian, max) =>
                           val r = repeatObs(librarian).filter(noloop(_))
                           r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                             if (max > 0) Observable(Right(librarian)) ++ Observable(Left(librarian -> (max - 1)))
                             else Observable(Right(librarian))
                           }
                       }
                     }
               }
             case None =>
               step.until
                 .filter(_.stepsList.nonEmpty)
//                 .filter(_.segmentList.head.stepsList.nonEmpty)
                 .map(traversalToF) match {
                 case Some(untilObs) =>
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable.tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                         case (librarian, max) =>
                           val r = repeatObs(librarian).filter(noloop(_))
                           r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                             untilObs(librarian).nonEmpty.flatMap {
                               case true => Observable(Right(librarian))
                               case false =>
                                 if (max > 0) Observable(Right(librarian)) ++ Observable(Left(librarian -> (max - 1)))
                                 else Observable(Right(librarian))
                             }
                           }
                       }
                     }
                 case None =>
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable
                         .tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                           case (librarian, max) =>
                             val r = repeatObs(librarian).filter(noloop(_))
                             r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                               if (max > 0) Observable(Right(librarian)) ++ Observable(Left(librarian -> (max - 1)))
                               else Observable(Right(librarian))
                             }
                         }
                     }
               }
           }
         } else {
           step.max match {
             case Some(max) =>
               step.until
                 .filter(_.stepsList.nonEmpty)
//                 .filter(_.segmentList.head.stepsList.nonEmpty)
                 .map(traversalToF) match {
                 case Some(untilObs) =>
                   scribe.trace("max until")
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable.tailRecM(librarian -> max) {
                         case (librarian, max) =>
                           val r = repeatObs(librarian).filter(noloop(_))
                           r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                             untilObs(librarian).nonEmpty.flatMap {
                               case true => Observable(Right(librarian))
                               case false =>
                                 if (max > 0) Observable(Left(librarian -> (max - 1))) else Observable(Right(librarian))
                             }
                           }
                       }
                     }
                 case None =>
                   scribe.trace("max")
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable.tailRecM(librarian -> max) {
                         case (librarian, max) =>
                           val r = repeatObs(librarian).filter(noloop(_))
                           r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                             if (max > 0) Observable(Left(librarian -> (max - 1))) else Observable(Right(librarian))
                           }
                       }
                     }
               }
             case None =>
               step.until
                 .filter(_.stepsList.nonEmpty)
//                 .filter(_.segmentList.head.stepsList.nonEmpty)
                 .map(traversalToF) match {
                 case Some(untilObs) =>
                   scribe.trace("until")
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable.tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                         case (librarian, max) =>
                           val r = repeatObs(librarian).filter(noloop(_))
                           r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                             untilObs(librarian).nonEmpty.flatMap {
                               case true => Observable(Right(librarian))
                               case false =>
                                 if (max > 0) Observable(Left(librarian -> (max - 1))) else Observable(Right(librarian))
                             }
                           }
                       }
                     }
                 case None =>
                   scribe.trace("last")
                   obs: Observable[Librarian[Any]] =>
                     obs.flatMap { librarian =>
                       Observable.tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                         case (librarian, max) =>
                           val r = repeatObs(librarian).filter(noloop(_))
                           r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                             if (max > 0) Observable(Left(librarian -> (max - 1))) else Observable(Right(librarian))
                           }
                       }
                     }
               }
           }
         }).andThen(_.delayOnNext(1.nanosecond))
      case step: Union[_, _] =>
        val unionObs = step.traversals.map(traversalToF).zip(step.traversals.map(_.stepsList.lastOption))
        obs: Observable[Librarian[Any]] =>
          {
            obs.flatMap { librarian =>
              unionObs
                .map {
                  case (f, Some(Head)) => f(librarian).head
                  case (f, Some(Last)) => f(librarian).last
                  case (f, step)       => f(librarian)
                }
                .reduce(_ ++ _)
            }
          }
    }
    f.asInstanceOf[Observable[Librarian[Any]] => Observable[Librarian[Any]]] andThen nextStep
  }

  def collectingBarrierStep(step: GroupingBarrierStep, steps: List[Step], isRootGroup: Boolean = false)(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {

    val nextStep = //buildNextStep(steps, segments)
      steps match {
        case List(step: ReducingStep) => reducingStep[Any](step)
        case List(step: ClipStep)     => clipStep[Any](step)
        case head :: tail =>
          obs: Observable[Librarian[Any]] =>
            Observable.raiseError(new Exception("GroupStep can only be followed by a ClipStep (currently)"))
        case Nil =>
          obs: Observable[Librarian[Any]] =>
            obs
      }

    val f = step match {
      case step: Group[_, _, _, _] =>
        val byObservable    = traversalToF(step.by)
        val mapBy           = tweakEnd(step.by)
        val valueObservable = traversalsToF(step.value)
        val mapValue        = tweakEnd(step.value)
        val valueSteps      = step.value.stepsList //.flatMap(_.stepsList)

        obs: Observable[Librarian[Any]] =>
          obs
            .mapEval { librarian =>
              mapBy(byObservable(librarian)).asInstanceOf[Task[Librarian[Any]]].map(_.get).map(librarian -> _)
            }
            .groupBy(
              l => l._2
            )
            .mapEval { group =>
              (if (step.value.stepsList.isEmpty) toList(valueObservable(group.map(_._1)))
               else mapValue(valueObservable(group.map(_._1))))
                .asInstanceOf[Task[Librarian[Any]]]
                .map(l => group.key -> l.get)
            }
            .map(createLibrarian(_))

    }
    f andThen nextStep
//    else
//      f andThen nextStep andThen (obs =>
//        Observable.fromTask(obs.toListL.map(_.asInstanceOf[List[(Any, Any)]].toMap).map()))
  }

  def countStep(step: Count, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: Count =>
        obs: Observable[Librarian[Any]] =>
          obs.count.map(createLibrarian(_))
    }
    f andThen nextStep
  }
  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: Mean =>
        obs: Observable[Librarian[Any]] =>
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
              case v: Long   => v
            }
          (for {
            sum   <- temp.sum
            count <- temp.count
          } yield {
            if (count > 0) Some(sum / count) else None
          }).flatMap(_.map(createLibrarian(_)).map(Observable(_)).getOrElse(Observable.empty[Librarian[Any]]))
      case step: Sum =>
        obs: Observable[Librarian[Any]] =>
          Observable
            .fromTask(
              obs
                .map(_.get)
//            .map {
//              case l: Librarian[Any] => l.get
//              case v                 => v
//            }
                .map {
                  case r: Resource[Any] => r.value
                  case v                => v
                }
                .collect {
                  case v: Int    => v
                  case v: Double => v
                  case v: Long   => v
                }
                .toListL
                .map {
                  case Nil  => None
                  case list => Some(list.sum)
                }
//              .map(_.sum)
//              .map(createLibrarian(_))
            )
            .flatMap(_.map(createLibrarian(_)).map(Observable(_)).getOrElse(Observable.empty[Librarian[Any]]))
    }
    f andThen nextStep
  }

//  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step])(
//      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
//    val nextStep = buildNextStep(steps)
//
//    val f = step match {
//
//    }
//    f andThen nextStep
//  }

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: Order =>
        val byObservable = traversalToF(step.by)
        val byObsF = (obs: Observable[Librarian[Any]]) =>
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
          case lspace.NS.types.`@string` =>
            val ordering = if (step.increasing) Ordering.String else Ordering.String.reverse
            byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[String])(ordering))))
          case lspace.NS.types.`@int` =>
            val ordering = if (step.increasing) Ordering.Int else Ordering.Int.reverse
            byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[Int])(ordering))))
          case lspace.NS.types.`@double` =>
            val ordering = if (step.increasing) Ordering.Double else Ordering.Double.reverse
            byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[Double])(ordering))))
          case lspace.NS.types.`@long` =>
            val ordering = if (step.increasing) Ordering.Long else Ordering.Long.reverse
            byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[Long])(ordering))))
          case lspace.NS.types.`@number` =>
            val ordering = if (step.increasing) Ordering.Double else Ordering.Double.reverse
            byObsF andThen (obs =>
              Observable.fromTask(obs.toListL.map(_.sortBy(_._2 match {
                case v: Int    => v.toDouble
                case v: Double => v
                case v: Long   => v.toDouble
              })(ordering))))
          case lspace.NS.types.`@datetime` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: Instant), (l2, v2: Instant)) => v1.isBefore(v2)
                })))
            } else {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: Instant), (l2, v2: Instant)) => v1.isAfter(v2)
                })))
            }
          case lspace.NS.types.`@localdatetime` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: LocalDateTime), (l2, v2: LocalDateTime)) => v1.isBefore(v2)
                })))
            } else {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: LocalDateTime), (l2, v2: LocalDateTime)) => v1.isAfter(v2)
                })))
            }
          case lspace.NS.types.`@date` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: LocalDate), (l2, v2: LocalDate)) => v1.isBefore(v2)
                })))
            } else {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: LocalDate), (l2, v2: LocalDate)) => v1.isAfter(v2)
                })))
            }
          case lspace.NS.types.`@time` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: LocalTime), (l2, v2: LocalTime)) => v1.isBefore(v2)
                })))
            } else {
              byObsF andThen (obs =>
                Observable.fromTask(obs.toListL.map(_.sortWith {
                  case ((l1, v1: LocalTime), (l2, v2: LocalTime)) => v1.isAfter(v2)
                })))
            }
        }).andThen(_.flatMap(Observable.fromIterable(_)).map(_._1))
    }
    f andThen nextStep
  }

  def projectionStep(step: ProjectionStep, steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: MapStep =>
        step match {
          case step: OutMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task
                      .gather(
                        r.outEMap(step.label.toList: _*)
                          .map {
                            case (property, edges) =>
                              nextStep(Observable.fromIterable(edges.map { e =>
                                librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to))
                              })).toListL.map(property -> _)
                          })
                      .map(_.toMap)
                      .map(librarian.copy(_))
                  case v => Task.now(librarian.copy(Map()))
                }))
          case step: OutEMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task
                      .gather(
                        r.outEMap(step.label.toList: _*)
                          .map {
                            case (property, edges) =>
                              nextStep(Observable.fromIterable(edges.map { e =>
                                librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e))
                              })).toListL.map(property -> _)
                          })
                      .map(_.toMap)
                      .map(librarian.copy(_))
                  case v => Task.now(librarian.copy(Map()))
                }))
          case step: InMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task
                      .gather(
                        r.inEMap(step.label.toList: _*)
                          .map {
                            case (property, edges) =>
                              nextStep(Observable.fromIterable(edges.map { e =>
                                librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from))
                              })).toListL.map(property -> _)
                          })
                      .map(_.toMap)
                      .map(librarian.copy(_))
                  case v => Task.now(librarian.copy(Map()))
                }))
          case step: InEMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task
                      .gather(
                        r.inEMap(step.label.toList: _*)
                          .map {
                            case (property, edges) =>
                              nextStep(Observable.fromIterable(edges.map { e =>
                                librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e))
                              })).toListL.map(property -> _)
                          })
                      .map(_.toMap)
                      .map(librarian.copy(_))
                  case v => Task.now(librarian.copy(Map()))
                }))
        }
      case step: Project[_] =>
        projectStep(step, steps)
      case step: Path[_, _] =>
        val byObs = traversalToF(step.by)
        step.by.stepsList.lastOption match {
          case Some(Count) =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(
                librarian =>
                  Observable.fromTask(
                    Task
                      .gather(librarian.path.resources.map { r =>
                        byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).headL
                      })
                      .map { v =>
                        v.map {
                          case librarian: Librarian[Any] =>
                            librarian.get match {
                              case r: Resource[_] => r.value
                              case v              => v
                            }
                        }
                      }))
          case Some(step @ (_: Head | _: Min | _: Max | _: Mean)) =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(
                librarian =>
                  Observable.fromTask(
                    Task
                      .gather(librarian.path.resources.map { r =>
                        byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).headOptionL
                      })
                      .map { v =>
                        v.map {
                          case Some(librarian: Librarian[Any] @unchecked) =>
                            Some(librarian.get).map {
                              case r: Resource[_] => r.value
                              case v              => v
                            }
                          case None => None
                          case t =>
                            throw new Exception(
                              s"unexpected content, expected an Librarian! found ${t.getClass.getSimpleName}")
                        }
                      }))
          case Some(Last) =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(
                librarian =>
                  Observable.fromTask(
                    Task
                      .gather(librarian.path.resources.map(r =>
                        byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).lastOptionL))
                      .map { v =>
                        v.map {
                          case Some(librarian: Librarian[Any]) =>
                            Some(librarian.get).map {
                              case r: Resource[_] => r.value
                              case v              => v
                            }
                          case None => None
                          case t =>
                            throw new Exception(
                              s"unexpected content, expected an Librarian! found ${t.getClass.getSimpleName}")
                        }
                      }))
          case _ =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(
                librarian =>
                  Observable.fromTask(
                    Task
                      .gather(librarian.path.resources.map(r => byObs(createLibrarian(r)).toListL))
                      .map { v =>
                        v.map {
                          _.map {
                            case librarian: Librarian[Any] =>
                              librarian.get match {
                                case r: Resource[_] => r.value
                                case v              => v
                              }
//                            case None => None
                            case t =>
                              throw new Exception(
                                s"unexpected content, expected an Librarian! found ${t.getClass.getSimpleName}")
                          }
                        }
                      }))
        }
    }
    f andThen { r =>
      r.map {
        case l: Librarian[Any] => l
        case other             => createLibrarian(other)
      }
    } andThen nextStep
  }
  def projectStep[Traversals <: HList](step: Project[Traversals], steps: List[Step])(
      implicit graph: Graph): Observable[Librarian[Any]] => Observable[Librarian[Any]] = {

    val nextStep = buildNextStep(steps)
    val pObs = step.by.runtimeList.reverse.map {
      case traversal: Traversal[ClassType[Any], ClassType[Any], HList] @unchecked =>
        traversalToF(traversal) -> (if (traversal.stepsList.isEmpty) { observable: Observable[Librarian[Any]] =>
                                      head(observable)
                                    } else tweakEnd(traversal))
    }
    val f =
      (obs: Observable[Librarian[Any]]) =>
        obs.flatMap { librarian =>
          Observable
            .fromTask(Task.gather(pObs.map {
              case (pOb, endMap) => endMap(pOb(librarian)).asInstanceOf[Task[Librarian[Any]]].map(_.get)
            }))
            .map {
              case List(v1)                                 => v1
              case List(v1, v2)                             => (v1, v2)
              case List(v1, v2, v3)                         => (v1, v2, v3)
              case List(v1, v2, v3, v4)                     => (v1, v2, v3, v4)
              case List(v1, v2, v3, v4, v5)                 => (v1, v2, v3, v4, v5)
              case List(v1, v2, v3, v4, v5, v6)             => (v1, v2, v3, v4, v5, v6)
              case List(v1, v2, v3, v4, v5, v6, v7)         => (v1, v2, v3, v4, v5, v6, v7)
              case List(v1, v2, v3, v4, v5, v6, v7, v8)     => (v1, v2, v3, v4, v5, v6, v7, v8)
              case List(v1, v2, v3, v4, v5, v6, v7, v8, v9) => (v1, v2, v3, v4, v5, v6, v7, v8, v9)
            }
            .map(librarian.copy(_))
      }

    f andThen nextStep
  }
}
