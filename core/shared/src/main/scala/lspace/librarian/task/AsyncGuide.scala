package lspace.librarian.task

import java.time._

import lspace.librarian.logic.Assistent
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal.step._
import lspace.librarian.traversal._
import lspace.structure._
import monix.eval.Task
import monix.reactive.Observable
import shapeless.HList

import scala.collection.mutable

object AsyncGuide {
  def apply()(implicit _assistent: Assistent): AsyncGuide = new AsyncGuide {
    val assistent: Assistent = _assistent
  }
}

trait AsyncGuide extends Guide[Observable] {

  def buildTraversal[Out](segments: List[Segment[_]]): Graph => Observable[Out] = { implicit graph: Graph =>

    segments match {
      case Nil => Observable.empty[Out]
      case segment :: segments =>
        segment.stepsList match {
          case Nil => Observable.empty[Out]
          case step :: steps =>
            val nextStep = buildNextStep(segment.stepsList, segments)
            nextStep(Observable(createLibrarian[Any](null)))
        }
    }
  }.andThen(_.map {
    case librarian: Librarian[Any] => toValue(librarian.get)
    case v => toValue(v)
  }.asInstanceOf[Observable[Out]])

  def traversalToF(segments: List[Segment[_]])(implicit graph: Graph): Librarian[Any] => Observable[Any] = {
    segments match {
      case Nil => librarian: Librarian[Any] => Observable(librarian)
      case segment :: segments =>
        val nextStep = buildNextStep(segment.stepsList, segments)
        librarian: Librarian[Any] => nextStep(Observable(librarian))
    }
  }

  def buildNextStep(steps: List[Step],
                    segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val (nextSteps, nextSegments) =
      if (steps.nonEmpty) steps -> segments
      else {
        segments match {
          case Nil => Nil -> Nil
          case segment :: segments =>
            segment.stepsList -> segments
        }
      }
    nextSteps match {
      case Nil => obs: Observable[Librarian[Any]] => obs
      case step :: steps =>
        step match {
          case step: ResourceStep =>
            resourceStep(step, steps, nextSegments)
          case step: MoveStep =>
            moveStep(step, steps, nextSegments)
          case step: FilterStep =>
            step match {
              case step: FilterBarrierStep =>
                filterBarrierStep(step, steps, nextSegments)
              case _ => filterStep(step, steps, nextSegments)
            }
          case step: ClipStep =>
            clipStep(step, steps, nextSegments)
          case step: BranchStep =>
            branchStep(step, steps, nextSegments)
          case step: CollectingBarrierStep =>
            collectingBarrierStep(step, steps, nextSegments)
          case step: RearrangeBarrierStep =>
            rearrangeBarrierStep(step, steps, nextSegments)
          case step: ReducingBarrierStep =>
            reducingBarrierStep(step, steps, nextSegments)
          case step: Project[_] =>
            projectStep(step, steps, nextSegments)
          case step: EnvironmentStep => step match {
            case step: TimeLimit =>
              val nextStep = buildNextStep(steps, nextSegments)
              import scala.concurrent.duration._
              val f = (obs: Observable[Librarian[Any]]) =>
                step.time match {
                  case Some(time) => obs.takeByTimespan(time.millis millis)
                  case None => obs
                }
              f andThen nextStep
          }
        }
    }
  }

  def resourceStep(step: ResourceStep,
                   steps: List[Step],
                   segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: N =>
        if (step.nodes.forall {
          case node: Node => node.graph == this
        }) {
          step.nodes match {
            case List() =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  Observable.fromIterable(graph.nodes().map(node => librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node))))
                }
            case list: List[Node] =>
              obs: Observable[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  Observable.fromIterable(list.map(node => librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node))))
                }
          }
        } else {
          obs: Observable[Librarian[Any]] =>
            obs.flatMap { librarian =>
              Observable.fromIterable(graph.nodes().filter(step.nodes.contains).map(node => librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node))))
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
                        Observable.fromIterable(graph.edges().map(edge => librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge))))
                      }
                  case list: List[Edge[_, _]] =>
                    obs: Observable[Librarian[Any]] =>
                      obs.flatMap { librarian =>
                        Observable.fromIterable(list.map(edge => librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge))))
                      }
                }
              } else {
                obs: Observable[Librarian[Any]] =>
                  obs.flatMap { librarian =>
                    Observable.fromIterable(graph.edges().filter(step.edges.contains).map(edge => librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge))))
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
                        Observable.fromIterable(graph.values().map(value => librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value))))
                      }
                  case list: List[Value[_]] =>
                    obs: Observable[Librarian[Any]] =>
                      obs.flatMap { librarian =>
                        Observable.fromIterable(list.map(value => librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value))))
                      }
                }
              } else {
                obs: Observable[Librarian[Any]] =>
                  obs.flatMap { librarian =>
                    Observable.fromIterable(graph.values().filter(v => step.values.contains(v.value)).map(value => librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value))))
                  }
              }
      //      case step: R =>

    }
    f andThen nextStep
  }

  def moveStep(step: MoveStep,
               steps: List[Step],
               segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {

    val nextStep = buildNextStep(steps, segments)
    step match {
      case step: MapStep =>
        step match {
          case step: OutMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task.gather(r.outEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        nextStep(Observable.fromIterable(edges.map(e => librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to))))).toListL.map(property -> _)
                      }).map(_.toMap)
                  case v => Task.now(Map())
                }))
          case step: OutEMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task.gather(r.outEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        nextStep(Observable.fromIterable(edges.map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e))))).toListL.map(property -> _)
                      }).map(_.toMap)
                  case v => Task.now(Map())
                }))
          case step: InMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task.gather(r.inEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        nextStep(Observable.fromIterable(edges.map(e => librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from))))).toListL.map(property -> _)
                      }).map(_.toMap)
                  case v => Task.now(Map())
                }))
          case step: InEMap =>
            obs: Observable[Librarian[Any]] =>
              obs.flatMap(librarian =>
                Observable.fromTask(librarian.get match {
                  case r: Resource[_] =>
                    Task.gather(r.inEMap(step.label.toList: _*)
                      .map { case (property, edges) =>
                        nextStep(Observable.fromIterable(edges.map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e))))).toListL.map(property -> _)
                      }).map(_.toMap)
                  case v => Task.now(Map())
                }))
          case step: Path[_,_]    =>
            val byObs = traversalToF(step.by.segmentList)
            step.by.steps.lastOption match {
              case Some(Head) =>
                obs: Observable[Librarian[Any]] =>
                  obs.flatMap(librarian =>
                    Observable.fromTask(Task.gather(librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).headOptionL)).map { v =>
                      v.map {
                        case Some(librarian: Librarian[Any]) => Some(librarian.get).map {
                          case r: Resource[_] => r.value
                          case v => v
                        }
                      }
                    })
                  )
              case Some(Last) =>
                obs: Observable[Librarian[Any]] =>
                  obs.flatMap(librarian =>
                    Observable.fromTask(Task.gather(librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).lastOptionL)).map { v =>
                      v.map {
                        case Some(librarian: Librarian[Any]) => Some(librarian.get).map {
                          case r: Resource[_] => r.value
                          case v => v
                        }
                      }
                    })
                  )
              case _ =>
                obs: Observable[Librarian[Any]] =>
                  obs.flatMap(librarian =>
                    Observable.fromTask(Task.gather(librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).toListL)).map { v =>
                      v
                    })
                  )
            }
        }
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
                r.labels.map(label => librarian.copy(label, path = librarian.path.copy(librarian.path.resources :+ label)))
              case v => List()
            })))
      case step: Id => obs: Observable[Librarian[Any]] =>
        nextStep(obs.collect { case librarian if librarian.get.isInstanceOf[Resource[_]] => librarian.copy(librarian.get.asInstanceOf[Resource[_]].id) })
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
    }
  }

  def filterStep(step: FilterStep,
                 steps: List[Step],
                 segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: HasStep =>
        step match {
          case step: Has =>
            step.predicate.fold { obs: Observable[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).nonEmpty
                  case v => false
                }
              }
            } { p: P[_] =>
              val helper = assistent.pToHelper(p)
              obs: Observable[Librarian[Any]] =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case v => false
                  }
                }
            }
          case step: HasNot =>
            step.predicate.fold { obs: Observable[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).isEmpty
                  case v => true
                }
              }
            } { p: P[_] =>
              val helper = assistent.pToHelper(p)
              obs: Observable[Librarian[Any]] =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => !r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case v => true
                  }
                }
            }
          case step: HasId => obs: Observable[Librarian[Any]] => obs.filter(_.get match {
            case r: Resource[_] if step.ids.contains(r.id) => true
            case _ => false
          })
          case step: HasIri => obs: Observable[Librarian[Any]] => obs.filter(_.get match {
            case r: Resource[_] if step.iris intersect r.iris nonEmpty => true
            case _ => false
          })
          case step: HasLabel =>
            obs: Observable[Librarian[Any]] => obs.filter(_.get match {
              case r: Resource[_] if step.label.exists(r.hasLabel(_).isDefined) => true
              case _ => false
            })
          case step: HasValue =>
            val helper = assistent.pToHelper(step.predicate)
            obs: Observable[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => helper.comparable(r.value) && helper.assert(r.value)
                  case v => helper.assert(v)
                }
              }
        }
      case step: Dedup =>
        import cats.Eq
        implicit val eqFoo: Eq[Any] = Eq.fromUniversalEquals
        obs: Observable[Librarian[Any]] =>
          Observable.fromTask(
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
          ).mergeMap(l => Observable.fromIterable(l))
      //              obj.groupBy(_.get) //BUG: does not filter like .distinct... why?!
      //              obs.groupBy(_.get match {
      //                case r: Resource[_] => r.value
      //                case v => v
      //              }).map(_.head).flatten
      case step: And =>
        val andObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            andObs.map(_ (librarian)).map(_.nonEmpty).reduce(_ ++ _).forall(b => b).flatMap {
              case true => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Or =>
        val orObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            orObs.map(_ (librarian)).map(_.nonEmpty).reduce(_ ++ _).exists(b => b).flatMap {
              case true => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Where =>
        val traveralObservable = traversalToF(step.traversal.segmentList)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            traveralObservable(librarian).nonEmpty.flatMap {
              case true => Observable(librarian)
              case false => Observable.empty
            }
          }
      case step: Not =>
        val traveralObservable = traversalToF(step.traversal.segmentList)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            traveralObservable(librarian).isEmpty.flatMap {
              case true => Observable(librarian)
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
              case v => helper.assert(v)
            }
          }
    }
    f andThen nextStep
  }

  //  def hasStep(step: HasStep, steps: List[Step], segments: List[Segment[_]])(
  //      obs: Observable[Librarian[Any]]): Observable[Librarian[Any]]
  def clipStep(step: ClipStep,
               steps: List[Step],
               segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Head => obs: Observable[Librarian[Any]] => obs.head
      case step: Last => obs: Observable[Librarian[Any]] => obs.last
      case step: Limit => obs: Observable[Librarian[Any]] => obs.take(step.max)
      case step: Range => obs: Observable[Librarian[Any]] => obs.drop(step.low).take(step.high - step.low)
      case step: Tail => obs: Observable[Librarian[Any]] => obs.takeLast(step.max)
    }
    f andThen nextStep
  }

  def branchStep(step: BranchStep,
                 steps: List[Step],
                 segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Coalesce[_, _] =>
        val coalObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            coalObs.foldLeft(Observable.empty[Any]) {
              case (obs, coalObs) =>
                obs.nonEmpty.flatMap {
                  case true => obs
                  case false => coalObs(librarian)
                }
            }
          }
      case step: Local =>
        val traveralObservable = traversalToF(step.traversal.segmentList)
        obs: Observable[Librarian[Any]] =>
          obs.flatMap { librarian =>
            traveralObservable(librarian)
          }
      case step: Repeat[_] => //TODO: modify to take noloop-parameter into account
        val repeatObs = traversalToF(step.traversal.segmentList)
        if (step.collect) {
          step.max match {
            case Some(max) =>
              step.until.map(_.segmentList).filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty).map(traversalToF) match {
                case Some(untilObs) =>
                  scribe.trace("collect max until")
                  obs: Observable[Librarian[Any]] =>
                    obs.flatMap { librarian =>
                      Observable.tailRecM(librarian -> max) {
                        case (librarian, max) =>
                          val r = repeatObs(librarian)
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
                  scribe.trace("collect max")
                  obs: Observable[Librarian[Any]] =>
                    obs.flatMap { librarian =>
                      Observable.tailRecM(librarian -> max) {
                        case (librarian, max) =>
                          val r = repeatObs(librarian)
                          r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                            if (max > 0) Observable(Right(librarian)) ++ Observable(Left(librarian -> (max - 1)))
                            else Observable(Right(librarian))
                          }
                      }
                    }
              }
            case None =>
              step.until.map(_.segmentList).filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty).map(traversalToF) match {
                case Some(untilObs) =>
                  scribe.trace("collect until")
                  obs: Observable[Librarian[Any]] =>
                    obs.flatMap { librarian =>
                      Observable.tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                        case (librarian, max) =>
                          val r = repeatObs(librarian)
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
                  scribe.trace("collect")
                  obs: Observable[Librarian[Any]] =>
                    obs.flatMap { librarian =>
                      Observable.tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                        case (librarian, max) =>
                          val r = repeatObs(librarian)
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
              step.until.map(_.segmentList).filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty).map(traversalToF) match {
                case Some(untilObs) =>
                  scribe.trace("max until")
                  obs: Observable[Librarian[Any]] =>
                    obs.flatMap { librarian =>
                      Observable.tailRecM(librarian -> max) {
                        case (librarian, max) =>
                          val r = repeatObs(librarian)
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
                          val r = repeatObs(librarian)
                          r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                            if (max > 0) Observable(Left(librarian -> (max - 1))) else Observable(Right(librarian))
                          }
                      }
                    }
              }
            case None =>
              step.until.map(_.segmentList).filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty).map(traversalToF) match {
                case Some(untilObs) =>
                  scribe.trace("until")
                  obs: Observable[Librarian[Any]] =>
                    obs.flatMap { librarian =>
                      Observable.tailRecM(librarian -> 100) { //make configurable (fail-safe max-depth)
                        case (librarian, max) =>
                          val r = repeatObs(librarian)
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
                          val r = repeatObs(librarian)
                          r.collect { case librarian: Librarian[Any] => librarian }.flatMap { librarian =>
                            if (max > 0) Observable(Left(librarian -> (max - 1))) else Observable(Right(librarian))
                          }
                      }
                    }
              }
          }
        }
      case step: Union[_, _] =>
        val unionObs = step.traversals.map(_.segmentList).map(traversalToF).zip(step.traversals.map(_.steps.lastOption))
        obs: Observable[Librarian[Any]] =>
        {
          obs.flatMap { librarian =>
            unionObs.map {
              case (f, Some(Head)) => f(librarian).head
              case (f, Some(Last)) => f(librarian).last
              case (f, step) => f(librarian)
            }.reduce(_ ++ _)
          }
        }
    }
    f.asInstanceOf[Observable[Librarian[Any]] => Observable[Librarian[Any]]] andThen nextStep
  }

  def collectingBarrierStep(step: CollectingBarrierStep,
                            steps: List[Step],
                            segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {

    val nextStep = buildNextStep(steps, segments)
    step match {
      case step: Group[_,_] =>
        val byObservable = traversalToF(step.by.segmentList)
        step.by.steps.lastOption match {
          case Some(Head) =>
            obs: Observable[Librarian[Any]] =>
              obs
                .flatMap { librarian =>
                  Observable
                    .from(byObservable(librarian).headOptionL.map(librarian -> _))
                }
                .groupBy(_._2.collect { case librarian: Librarian[Any] => librarian }
                  .map(_.get match {
                  case resource: Resource[Any] => resource.value
                  case v: Any => v
                }))
                .flatMap{ group =>
                  nextStep(group.map(_._1)).map(group.key -> _)
                }
          case Some(Last) =>
            obs: Observable[Librarian[Any]] =>
              obs
                .flatMap { librarian =>
                  Observable
                    .from(byObservable(librarian).lastOptionL.map(librarian -> _))
                }
                .groupBy(_._2.collect { case librarian: Librarian[Any] => librarian }
                  .map(_.get match {
                  case resource: Resource[Any] => resource.value
                  case v: Any => v
                }))
                .flatMap{ group =>
                  nextStep(group.map(_._1)).map(group.key -> _)
                }
          case _ =>
            obs: Observable[Librarian[Any]] =>
              Observable.fromTask(
              obs
                .flatMap { librarian =>
                  Observable
                    .from(byObservable(librarian).toListL.map(librarian -> _))
                }
                .groupBy(_._2.collect { case librarian: Librarian[Any] => librarian }.map(_.get match {
                  case resource: Resource[Any] => resource.value
                  case v: Any => v
                }).toSet)
                .flatMap{ group =>
                  Observable.fromTask(
                  nextStep(group.map(_._1)).toListL.map(group.key.toList -> _)
                  )
                }.toListL.map(_.toMap))
//                .map(t => t._1.toList -> t._2)
//                .mapValues(_.map(_._1))
//                .mapValues(nextStep(_).toList))
        }
    }
  }

  def reducingBarrierStep(step: ReducingBarrierStep,
                          steps: List[Step],
                          segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Count => obs: Observable[Librarian[Any]] => obs.count.map(createLibrarian(_)).map(createLibrarian(_))
      case step: Mean => obs: Observable[Librarian[Any]] =>
        val temp = obs.map(_.get).map {
          case l: Librarian[Any] => l.get
          case v => v
        }.map{
          case r: Resource[Any] => r.value
          case v => v
        }.collect {
          case v: Int => v
          case v: Double => v
          case v: Long => v
        }
        (for {
          sum <- temp.sum
          count <- temp.count
        } yield {
          if (count == 0) Double.NaN else sum / count
        }).map(createLibrarian(_))
      case step: Sum => obs: Observable[Librarian[Any]] =>
        obs
          .map(_.get).map {
          case l: Librarian[Any] => l.get
          case v => v
        }.map{
          case r: Resource[Any] => r.value
          case v => v
        }
          .collect {
            case v: Int => v
            case v: Double => v
            case v: Long => v
          }
          .sum
          .map(createLibrarian(_))
    }
    f andThen nextStep
  }

  def filterBarrierStep(step: FilterBarrierStep,
                        steps: List[Step],
                        segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Min =>
        val byObservable = traversalToF(step.by.segmentList)
        val byObsF = (obs: Observable[Librarian[Any]]) =>
          obs
            .flatMap { librarian =>
              byObservable(librarian).map {
                case l: Librarian[Any] => l.get
                case v => v
              }.map{
                case resource: Resource[Any] => resource.value
                case v: Any => v
              }.map(librarian -> _)
            }
        import cats.implicits._
        (step.by.et.iri match {
          case lspace.NS.types.`@int` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[Int]))
          case lspace.NS.types.`@double` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[Double]))
          case lspace.NS.types.`@long` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[Long]))
          case lspace.NS.types.`@number` =>
            byObsF andThen(_.minBy(_._2 match {
              case v: Int => v.toDouble
              case v: Double => v
              case v: Long => v.toDouble
            }))
          case lspace.NS.types.`@datetime` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[Instant].toEpochMilli))
          case lspace.NS.types.`@localdatetime` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[LocalDateTime].toEpochSecond(ZoneOffset.UTC)))
          case lspace.NS.types.`@date` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[LocalDate].toEpochDay))
          case lspace.NS.types.`@time` =>
            byObsF andThen(_.minBy(_._2.asInstanceOf[LocalTime].toNanoOfDay))
        }).andThen(_.map(_._1))
      case step: Max =>
        val byObservable = traversalToF(step.by.segmentList)
        val byObsF = (obs: Observable[Librarian[Any]]) =>
          obs
            .flatMap { librarian =>
              byObservable(librarian).map {
                case l: Librarian[Any] => l.get
                case v => v
              }.map{
                case resource: Resource[Any] => resource.value
                case v: Any => v
              }.map(librarian -> _)
            }
        import cats.implicits._
        (step.by.et.iri match {
          case lspace.NS.types.`@int` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[Int]))
          case lspace.NS.types.`@double` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[Double]))
          case lspace.NS.types.`@long` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[Long]))
          case lspace.NS.types.`@number` =>
            byObsF andThen(_.maxBy(_._2 match {
              case v: Int => v.toDouble
              case v: Double => v
              case v: Long => v.toDouble
            }))
          case lspace.NS.types.`@datetime` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[Instant].toEpochMilli))
          case lspace.NS.types.`@localdatetime` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[LocalDateTime].toEpochSecond(ZoneOffset.UTC)))
          case lspace.NS.types.`@date` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[LocalDate].toEpochDay))
          case lspace.NS.types.`@time` =>
            byObsF andThen(_.maxBy(_._2.asInstanceOf[LocalTime].toNanoOfDay))
        }).andThen(_.map(_._1))
    }
    f andThen nextStep
  }

  def rearrangeBarrierStep(step: RearrangeBarrierStep,
                           steps: List[Step],
                           segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Order =>
        val byObservable = traversalToF(step.by.segmentList)
        val byObsF = (obs: Observable[Librarian[Any]]) =>
          obs
            .flatMap { librarian =>
              byObservable(librarian).map {
                case l: Librarian[Any] => l.get
                case v => v
              }.map{
                case resource: Resource[Any] => resource.value
                case v: Any => v
              }.map(librarian -> _)
            }
        import cats.implicits._
        (step.by.et.iri match {
          case lspace.NS.types.`@string` =>
            val ordering = if (step.increasing) Ordering.String else Ordering.String.reverse
            byObsF andThen(obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[String])(ordering))))
          case lspace.NS.types.`@int` =>
            val ordering = if (step.increasing) Ordering.Int else Ordering.Int.reverse
            byObsF andThen(obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[Int])(ordering))))
          case lspace.NS.types.`@double` =>
            val ordering = if (step.increasing) Ordering.Double else Ordering.Double.reverse
            byObsF andThen(obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[Double])(ordering))))
          case lspace.NS.types.`@long` =>
            val ordering = if (step.increasing) Ordering.Long else Ordering.Long.reverse
            byObsF andThen(obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2.asInstanceOf[Long])(ordering))))
          case lspace.NS.types.`@number` =>
            val ordering = if (step.increasing) Ordering.Double else Ordering.Double.reverse
            byObsF andThen(obs => Observable.fromTask(obs.toListL.map(_.sortBy(_._2 match {
              case v: Int => v.toDouble
              case v: Double => v
              case v: Long => v.toDouble
            })(ordering))))
          case lspace.NS.types.`@datetime` =>
            if (step.increasing) {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: Instant), (l2, v2: Instant)) => v1.isBefore(v2)
              })))
            } else {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: Instant), (l2, v2: Instant)) => v1.isAfter(v2)
              })))
            }
          case lspace.NS.types.`@localdatetime` =>
            if (step.increasing) {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: LocalDateTime), (l2, v2: LocalDateTime)) => v1.isBefore(v2)
              })))
            } else {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: LocalDateTime), (l2, v2: LocalDateTime)) => v1.isAfter(v2)
              })))
            }
          case lspace.NS.types.`@date` =>
            if (step.increasing) {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: LocalDate), (l2, v2: LocalDate)) => v1.isBefore(v2)
              })))
            } else {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: LocalDate), (l2, v2: LocalDate)) => v1.isAfter(v2)
              })))
            }
          case lspace.NS.types.`@time` =>
            if (step.increasing) {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: LocalTime), (l2, v2: LocalTime)) => v1.isBefore(v2)
              })))
            } else {
              byObsF andThen (obs => Observable.fromTask(obs.toListL.map(_.sortWith {
                case ((l1, v1: LocalTime), (l2, v2: LocalTime)) => v1.isAfter(v2)
              })))
            }
        }).andThen(_.flatMap(Observable.fromIterable(_)).map(_._1))
    }
    f andThen nextStep
  }

  def projectStep[Traversals <: HList](step: Project[Traversals],
                                       steps: List[Step],
                                       segments: List[Segment[_]])(implicit graph: Graph): Observable[Librarian[Any]] => Observable[Any] = {

    val nextStep = buildNextStep(steps, segments)
    val pObs = step.by.runtimeList.map {
      case traversal: Traversal[ClassType[Any], ClassType[Any], HList] =>
        traversalToF(traversal.segmentList) -> traversal.steps.lastOption
    }
   val f =
     (obs: Observable[Librarian[Any]]) =>
      obs.flatMap { librarian =>
        Observable.fromTask(Task.gather(pObs.map {
          case (pOb, Some(Head)) => pOb(librarian).headL
          case (pOb, Some(Last)) => pOb(librarian).lastL
          case (pOb, _)          => pOb(librarian).toListL
        })).map {
          case List(v1) => v1
          case List(v1, v2) => (v1, v2)
          case List(v1, v2, v3) => (v1, v2, v3)
          case List(v1, v2, v3, v4) => (v1, v2, v3, v4)
          case List(v1, v2, v3, v4, v5) => (v1, v2, v3, v4, v5)
          case List(v1, v2, v3, v4, v5, v6) => (v1, v2, v3, v4, v5, v6)
        }.map(librarian.copy(_))
      }
//    val f = step.by match {
//      case List(p1, p2) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        obs: Observable[Librarian[Any]] =>
//          obs.flatMap { librarian =>
//            Observable.fromTask(for {
//              v1 <- p1Obs(librarian).toListL
//              v2 <- p2Obs(librarian).toListL
//            } yield {
//              librarian.copy((v1, v2))
//            })
//          }
//      case List(p1, p2, p3) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        val p3Obs = traversalToF(p3.segmentList)
//        obs: Observable[Librarian[Any]] =>
//          obs.flatMap { librarian =>
//            Observable.fromTask(for {
//              v1 <- p1Obs(librarian).toListL
//              v2 <- p2Obs(librarian).toListL
//              v3 <- p3Obs(librarian).toListL
//            } yield {
//              librarian.copy((v1, v2, v3))
//            })
//          }
//      case List(p1, p2, p3, p4) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        val p3Obs = traversalToF(p3.segmentList)
//        val p4Obs = traversalToF(p4.segmentList)
//        obs: Observable[Librarian[Any]] =>
//          obs.flatMap { librarian =>
//            Observable.fromTask(for {
//              v1 <- p1Obs(librarian).toListL
//              v2 <- p2Obs(librarian).toListL
//              v3 <- p3Obs(librarian).toListL
//              v4 <- p4Obs(librarian).toListL
//            } yield {
//              librarian.copy((v1, v2, v3, v4))
//            })
//          }
//    }

    f andThen nextStep
  }
}

