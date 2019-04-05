package lspace.librarian.task

import java.time._

import lspace.librarian.logic.Assistent

import scala.collection.mutable
import scala.util.Try
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal._
import lspace.librarian.task._
import lspace.librarian.traversal.step._
import lspace.structure._
import monix.eval.Coeval
import shapeless.{=:!=, HList, HNil, :: => ::::}

import scala.annotation.tailrec

object SyncGuide {
  def apply()(implicit _assistent: Assistent): SyncGuide = new SyncGuide {
    val assistent: Assistent = _assistent
  }
}
trait SyncGuide extends Guide[Stream] {

  implicit def segmentsToFlattenedSteps(segments: List[Segment[HList]]): List[Step] =
    segments.flatMap(_.stepsList)

  def buildTraversal[Out](segments: List[Segment[_]]): Graph => Stream[Out] = { implicit graph: Graph =>
    segments match {
      case Nil => Stream.empty[Out]
      case segment :: segments =>
        segment.stepsList match {
          case Nil => Stream.empty[Out]
          case step :: steps =>
            findFirstContainer((segment :: segments).flatMap(_.stepsList)) match {
              case Some(step: Group[_, _]) =>
                val (until, from) = (segment :: segments).flatMap(_.stepsList).span(_ != step)
                val nextStep = buildNextStep(until, Nil) andThen
                  collectingBarrierStep(step, from.tail, Nil, true).asInstanceOf[Stream[Any] => Stream[Any]]
                nextStep(Stream(createLibrarian[Any](null)))
              case step =>
                val nextStep = buildNextStep(segment.stepsList, segments)
                nextStep(Stream(createLibrarian[Any](null)))
            }
        }
    }
  }.andThen(_.map {
    case librarian: Librarian[Any] => toValue(librarian.get)
    case v                         => toValue(v)
  }.asInstanceOf[Stream[Out]])

  def traversalToF(segments: List[Segment[_]])(implicit graph: Lspace): Librarian[Any] => Stream[Any] = {
    segments match {
      case Nil =>
        librarian: Librarian[Any] =>
          Stream(librarian)
      case segment :: segments =>
        val nextStep = buildNextStep(segment.stepsList, segments)
        librarian: Librarian[Any] =>
          nextStep(Stream(librarian))
    }
  }

  def buildNextStep(steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val (nextSteps, nextSegments) =
      if (steps.nonEmpty) steps -> segments
      else {
        segments match {
          case Nil                 => Nil               -> Nil
          case segment :: segments => segment.stepsList -> segments
        }
      }
    nextSteps match {
      case Nil =>
        obs: Stream[Librarian[Any]] =>
          obs
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
          case step: EnvironmentStep =>
            step match {
              case step: TimeLimit =>
                val nextStep = buildNextStep(steps, nextSegments)
                ((obs: Stream[Librarian[Any]]) => obs) andThen nextStep
            }
        }
    }
  }

  def resourceStep(step: ResourceStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: N =>
        if (step.nodes.forall {
              case node: Node => node.graph == this
            }) {
          step.nodes match {
            case List() =>
              obs: Stream[Librarian[Any]] =>
                obs
                  .flatMap { librarian =>
                    graph.nodeStore.cached
                      .all()
                      .asInstanceOf[Stream[Node]]
                      .map(node =>
                        librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node)))
                  }
            case list: List[Node] =>
              obs: Stream[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  list.map(node =>
                    librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node)))
                }
          }
        } else { obs: Stream[Librarian[Any]] =>
          obs.flatMap { librarian =>
            graph.nodeStore.cached
              .all()
              .asInstanceOf[Stream[Node]]
              .filter(step.nodes.contains)
              .map(node => librarian.copy(get = node, path = librarian.path.copy(librarian.path.resources :+ node)))
          }
        }
      case step: E =>
        if (step.edges.forall {
              case edge: Edge[_, _] => edge.graph == this
            }) {
          step.edges match {
            case List() =>
              obs: Stream[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  graph.edgeStore.cached
                    .all()
                    .asInstanceOf[Stream[Edge[_, _]]]
                    .map(edge =>
                      librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge)))
                }
            case list: List[Edge[_, _]] =>
              obs: Stream[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  list.map(edge =>
                    librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge)))
                }
          }
        } else { obs: Stream[Librarian[Any]] =>
          obs.flatMap { librarian =>
            graph.edgeStore.cached
              .all()
              .asInstanceOf[Stream[Edge[_, _]]]
              .filter(step.edges.contains)
              .map(edge => librarian.copy(get = edge, path = librarian.path.copy(librarian.path.resources :+ edge)))
          }
        }
      case step: V =>
        if (step.values.forall {
              case edge: Value[_] => edge.graph == this
            }) {
          step.values match {
            case List() =>
              obs: Stream[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  graph.valueStore.cached
                    .all()
                    .asInstanceOf[Stream[Value[_]]]
                    .map(value =>
                      librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value)))
                }
            case list: List[Value[_]] =>
              obs: Stream[Librarian[Any]] =>
                obs.flatMap { librarian =>
                  list.map(value =>
                    librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value)))
                }
          }
        } else { obs: Stream[Librarian[Any]] =>
          obs.flatMap { librarian =>
            graph.valueStore.cached
              .all()
              .asInstanceOf[Stream[Value[_]]]
              .filter(v => step.values.contains(v.value))
              .map(value => librarian.copy(get = value, path = librarian.path.copy(librarian.path.resources :+ value)))
          }
        }
      //      case step: R =>

    }
    f andThen nextStep
  }

  def moveStep(step: MoveStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)
    step match {
      case step: MapStep =>
        step match {
          case step: OutMap =>
            obs: Stream[Librarian[Any]] =>
              obs.map(librarian =>
                librarian.get match {
                  case r: Resource[_] =>
                    r.outEMap(step.label.toList: _*)
                      .map {
                        case (property, edges) =>
                          property -> nextStep(edges.toStream.map(e =>
                            librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))).toList
                      }
                  case v => Map()
              })
          case step: OutEMap =>
            obs: Stream[Librarian[Any]] =>
              obs.map(librarian =>
                librarian.get match {
                  case r: Resource[_] =>
                    r.outEMap(step.label.toList: _*)
                      .map {
                        case (property, edges) =>
                          property -> nextStep(edges.toStream.map(e =>
                            librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))).toList
                      }
                  case v => Map()
              })
          case step: InMap =>
            obs: Stream[Librarian[Any]] =>
              obs.map(librarian =>
                librarian.get match {
                  case r: Resource[_] =>
                    r.inEMap(step.label.toList: _*)
                      .map {
                        case (property, edges) =>
                          property -> nextStep(
                            edges.toStream.map(e =>
                              librarian
                                .copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))).toList
                      }
                  case v => Map()
              })
          case step: InEMap =>
            obs: Stream[Librarian[Any]] =>
              obs.map(librarian =>
                librarian.get match {
                  case r: Resource[_] =>
                    r.inEMap(step.label.toList: _*)
                      .map {
                        case (property, edges) =>
                          property -> nextStep(edges.toStream.map(e =>
                            librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))).toList
                      }
                  case v => Map()
              })
          case step: Path[_, _] =>
            val byObs = traversalToF(step.by.segmentList)
            step.by.steps.lastOption match {
              case Some(Count) =>
                obs: Stream[Librarian[Any]] =>
                  obs.map { librarian =>
                    librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).head)
                  }
              case Some(step @ (_: Head | _: Min | _: Max | _: Mean)) =>
                obs: Stream[Librarian[Any]] =>
                  obs.map { librarian =>
                    librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).headOption)
                  }
              case Some(Last) =>
                obs: Stream[Librarian[Any]] =>
                  obs.map { librarian =>
                    librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).lastOption)
                  }
              case _ =>
                obs: Stream[Librarian[Any]] =>
                  obs.map { librarian =>
                    librarian.path.resources.map(r => byObs(createLibrarian(r.asInstanceOf[Resource[Any]])).toList)
                  }
            }
        }
      case step: Out =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case r: Resource[_] =>
                r.outE(step.label.toList: _*)
                  .map(e => librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))
              case v => List()
          }))
      case step: OutE =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case r: Resource[_] =>
                r.outE(step.label.toList: _*)
                  .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
              case v => List()
          }))
      case step: In =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case r: Resource[_] =>
                r.inE(step.label.toList: _*)
                  .map(e => librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))
              case v => List()
          }))
      case step: InE =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case r: Resource[_] =>
                r.inE(step.label.toList: _*)
                  .map(e => librarian.copy(e, path = librarian.path.copy(librarian.path.resources :+ e)))
              case v => List()
          }))
      case step: Label =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case r: Resource[_] =>
                r.labels.map(label =>
                  librarian.copy(label, path = librarian.path.copy(librarian.path.resources :+ label)))
              case v => List()
          }))
      case step: Id =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.collect {
            case librarian if librarian.get.isInstanceOf[Resource[_]] =>
              librarian.copy(librarian.get.asInstanceOf[Resource[_]].id)
          })
      case step: From =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case e: Edge[_, _] =>
                List(librarian.copy(e.from, path = librarian.path.copy(librarian.path.resources :+ e.from)))
              case v => List()
          }))
      case step: To =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.flatMap(librarian =>
            librarian.get match {
              case e: Edge[_, _] =>
                List(librarian.copy(e.to, path = librarian.path.copy(librarian.path.resources :+ e.to)))
              case v => List()
          }))
      case step: Constant[_, _, _] =>
        obs: Stream[Librarian[Any]] =>
          nextStep(obs.map(librarian => librarian.copy(step.value)))
    }
  }

  def filterStep(step: FilterStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: HasStep =>
        step match {
          case step: Has =>
            step.predicate.fold { obs: Stream[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).nonEmpty
                  case v              => false
                }
              }
            } { p: P[_] =>
              val helper = assistent.pToHelper(p)
              obs: Stream[Librarian[Any]] =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case v              => false
                  }
                }
            }
          case step: HasNot =>
            step.predicate.fold { obs: Stream[Librarian[Any]] =>
              obs.filter { librarian =>
                librarian.get match {
                  case r: Resource[_] => r.out(step.key).isEmpty
                  case v              => true
                }
              }
            } { p: P[_] =>
              val helper = assistent.pToHelper(p)
              obs: Stream[Librarian[Any]] =>
                obs.filter { librarian =>
                  librarian.get match {
                    case r: Resource[_] => !r.out(step.key).filter(helper.comparable).exists(helper.assert)
                    case v              => true
                  }
                }
            }
          case step: HasId =>
            obs: Stream[Librarian[Any]] =>
              obs.filter(_.get match {
                case r: Resource[_] if step.ids.contains(r.id) => true
                case _                                         => false
              })
          case step: HasIri =>
            obs: Stream[Librarian[Any]] =>
              obs.filter(_.get match {
                case r: Resource[_] if step.iris intersect r.iris nonEmpty => true
                case _                                                     => false
              })
          case step: HasLabel =>
            obs: Stream[Librarian[Any]] =>
              obs.filter(_.get match {
                case r: Resource[_] if step.label.exists(r.hasLabel(_).isDefined) => true
                case _                                                            => false
              })
          case step: HasValue =>
            val helper = assistent.pToHelper(step.predicate)
            obs: Stream[Librarian[Any]] =>
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
        obs: Stream[Librarian[Any]] =>
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
        val andObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Stream[Librarian[Any]] =>
          obs.filter { librarian =>
            andObs.forall(_(librarian).nonEmpty)
          }
      case step: Or =>
        val orObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Stream[Librarian[Any]] =>
          obs.filter { librarian =>
            orObs.exists(_(librarian).nonEmpty)
          }
      case step: Where =>
        val traveralObservable = traversalToF(step.traversal.segmentList)
        obs: Stream[Librarian[Any]] =>
          obs.filter { librarian =>
            traveralObservable(librarian).nonEmpty
          }
      case step: Not =>
        val traveralObservable = traversalToF(step.traversal.segmentList)
        obs: Stream[Librarian[Any]] =>
          obs.filter { librarian =>
            traveralObservable(librarian).isEmpty
          }
      case step: Coin =>
        obs: Stream[Librarian[Any]] =>
          obs.filter { librarian =>
            Math.random() < step.p //get next seeded random value
          }
      case step: Is =>
        val helper = assistent.pToHelper(step.predicate)
        obs: Stream[Librarian[Any]] =>
          obs.filter { librarian =>
            librarian.get match {
              case r: Resource[_] => helper.assert(r.value)
              case v              => helper.assert(v)
            }
          }
    }
    f andThen nextStep
  }

  def clipStep(step: ClipStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Head =>
        obs: Stream[Librarian[Any]] =>
          obs.take(1)
      case step: Last =>
        obs: Stream[Librarian[Any]] =>
          obs.lastOption.toStream
      case step: Limit =>
        obs: Stream[Librarian[Any]] =>
          obs.take(step.max)
      case step: Range =>
        obs: Stream[Librarian[Any]] =>
          obs.slice(step.low, step.high)
      case step: Tail =>
        obs: Stream[Librarian[Any]] =>
          obs.takeRight(step.max)
    }
    f andThen nextStep
  }

  def branchStep(step: BranchStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Coalesce[_, _] =>
        val coalObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Stream[Librarian[Any]] =>
          obs.flatMap { librarian =>
            coalObs.map(t => Coeval.evalOnce(t(librarian))).find(_.value().nonEmpty).toList
          }
      case step: Choose[_, _] =>
        val byObs    = traversalToF(step.by.segmentList)
        val rightObs = traversalToF(step.right.segmentList)
        val leftObs  = traversalToF(step.left.segmentList)
        obs: Stream[Librarian[Any]] =>
          obs.flatMap { librarian =>
            byObs(librarian).nonEmpty match {
              case true =>
                rightObs(librarian)
              case false =>
                leftObs(librarian)
            }
          }
      case step: Local =>
        val traveralObservable = traversalToF(step.traversal.segmentList)
        obs: Stream[Librarian[Any]] =>
          obs.flatMap { librarian =>
            traveralObservable(librarian)
          }
      case step: Repeat[_] => //TODO: modify to take noloop-parameter into account
        val repeatObs = traversalToF(step.traversal.segmentList)
        if (step.collect) {
          step.max match {
            case Some(max) =>
              step.until
                .map(_.segmentList)
                .filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(obs.collect {
                          case librarian: Librarian[Any] if untilObs(librarian).isEmpty =>
                            librarian
                        }, maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), max))
                case None =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(obs.collect {
                          case librarian: Librarian[Any] =>
                            librarian
                        }, maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), max))
              }
            case None =>
              step.until
                .map(_.segmentList)
                .filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(obs.collect {
                          case librarian: Librarian[Any] if untilObs(librarian).isEmpty =>
                            librarian
                        }, maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), 100)) //make configurable (fail-safe max-depth)
                case None =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        obs #::: repeat(obs.collect {
                          case librarian: Librarian[Any] =>
                            librarian
                        }, maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), 100)) //make configurable (fail-safe max-depth)
              }
          }
        } else {
          step.max match {
            case Some(max) =>
              step.until
                .map(_.segmentList)
                .filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        val (finished, notFinished) = obs.partition {
                          case librarian: Librarian[Any] if untilObs(librarian).nonEmpty => true
                          case _                                                         => false
                        }
                        finished #::: repeat(notFinished.asInstanceOf[Stream[Librarian[Any]]], maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), max))
                case None =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        repeat(obs.collect {
                          case librarian: Librarian[Any] =>
                            librarian
                        }, maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), max))
              }
            case None =>
              step.until
                .map(_.segmentList)
                .filter(_.nonEmpty)
                .filter(_.head.stepsList.nonEmpty)
                .map(traversalToF) match {
                case Some(untilObs) =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        val (finished, notFinished) = obs.partition {
                          case librarian: Librarian[Any] if untilObs(librarian).nonEmpty => true
                          case _                                                         => false
                        }
                        finished #::: repeat(notFinished.asInstanceOf[Stream[Librarian[Any]]], maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), 100)) //make configurable (fail-safe max-depth)
                case None =>
                  obs: Stream[Librarian[Any]] =>
                    def repeat(librarians: Stream[Librarian[Any]], max: Int): Stream[Any] = {
                      val obs = librarians.flatMap(repeatObs(_))
                      if (max > 0) {
                        val maxMinOne = max - 1
                        repeat(obs.collect {
                          case librarian: Librarian[Any] =>
                            librarian
                        }, maxMinOne)
                      } else obs
                    }
                    obs.flatMap(l => repeat(Stream(l), 100)) //make configurable (fail-safe max-depth)
              }
          }
        }
      case step: Union[_, _] =>
        val unionObs = step.traversals.map(_.segmentList).map(traversalToF)
        obs: Stream[Librarian[Any]] =>
          {
            obs.flatMap { librarian =>
              unionObs.map(_(librarian)).reduce(_ ++ _)
            }
          }
    }
    f.asInstanceOf[Stream[Librarian[Any]] => Stream[Librarian[Any]]] andThen nextStep
  }

  def collectingBarrierStep(
      step: CollectingBarrierStep,
      steps: List[Step],
      segments: List[Segment[_]],
      isRootGroup: Boolean = false)(implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)
    val f = step match {
      case step: Group[_, _] =>
        val byObservable = traversalToF(step.by.segmentList)
        step.by.steps.lastOption match {
          case Some(Count) =>
            obs: Stream[Librarian[Any]] =>
              obs
                .map { librarian =>
                  librarian -> byObservable(librarian).head
                }
                .groupBy(l =>
                  l._2.asInstanceOf[Librarian[Any]].get match {
                    case resource: Resource[Any] => resource.value
                    case v: Any                  => v
                })
                .mapValues(_.map(_._1))
          case Some(step @ (_: Head | _: Min | _: Max | _: Mean)) =>
            obs: Stream[Librarian[Any]] =>
              obs
                .map { librarian =>
                  librarian -> byObservable(librarian).headOption
                }
                .groupBy(_._2
                  .collect { case librarian: Librarian[Any] => librarian }
                  .map(_.get match {
                    case resource: Resource[Any] => resource.value
                    case v: Any                  => v
                  }))
                .mapValues(_.map(_._1))
          case Some(Last) =>
            obs: Stream[Librarian[Any]] =>
              obs
                .map { librarian =>
                  librarian -> byObservable(librarian).lastOption
                }
                .groupBy(_._2
                  .collect { case librarian: Librarian[Any] => librarian }
                  .map(_.get match {
                    case resource: Resource[Any] => resource.value
                    case v: Any                  => v
                  }))
                .mapValues(_.map(_._1))
          case _ =>
            obs: Stream[Librarian[Any]] =>
              obs
                .map { librarian =>
                  librarian -> byObservable(librarian).toList
                }
                .groupBy(_._2
                  .collect { case librarian: Librarian[Any] => librarian }
                  .map(_.get match {
                    case resource: Resource[Any] => resource.value
                    case v: Any                  => v
                  })
                  .toSet)
                .map(t => t._1.toList -> t._2)
                .mapValues(_.map(_._1))

        }
    }
    if (isRootGroup) {
      f andThen (_.toStream
        .map { case (k, v) => k -> nextStep(v).toList })
    } else f andThen (_.mapValues(nextStep(_).toList)) andThen (Stream(_))
  }

  def reducingBarrierStep(step: ReducingBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Count =>
        obs: Stream[Librarian[Any]] =>
          Stream(createLibrarian(obs.size.toLong))
      case step: Mean =>
        obs: Stream[Librarian[Any]] =>
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
          val sum   = temp.sum
          val count = temp.size

          Stream(createLibrarian(if (count == 0) Double.NaN else sum / count))
      case step: Sum =>
        obs: Stream[Librarian[Any]] =>
          Stream(
            createLibrarian(
              obs
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
                .sum))
    }
    f andThen nextStep
  }

  def filterBarrierStep(step: FilterBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Min =>
        val byObservable = traversalToF(step.by.segmentList)
        val byObsF = (obs: Stream[Librarian[Any]]) =>
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
        }).andThen(_._1).andThen(Stream(_))
      case step: Max =>
        val byObservable = traversalToF(step.by.segmentList)
        val byObsF = (obs: Stream[Librarian[Any]]) =>
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
        }).andThen(_._1).andThen(Stream(_))
    }
    f andThen nextStep
  }

  def rearrangeBarrierStep(step: RearrangeBarrierStep, steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val f = step match {
      case step: Order =>
        val byObservable = traversalToF(step.by.segmentList)
        val byObsF = (obs: Stream[Librarian[Any]]) =>
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
            byObsF andThen (obs => obs.sortBy(_._2.asInstanceOf[String])(ordering))
          case lspace.NS.types.`@int` =>
            val ordering = if (step.increasing) Ordering.Int else Ordering.Int.reverse
            byObsF andThen (obs => obs.sortBy(_._2.asInstanceOf[Int])(ordering))
          case lspace.NS.types.`@double` =>
            val ordering = if (step.increasing) Ordering.Double else Ordering.Double.reverse
            byObsF andThen (obs => obs.sortBy(_._2.asInstanceOf[Double])(ordering))
          case lspace.NS.types.`@long` =>
            val ordering = if (step.increasing) Ordering.Long else Ordering.Long.reverse
            byObsF andThen (obs => obs.sortBy(_._2.asInstanceOf[Long])(ordering))
          case lspace.NS.types.`@number` =>
            val ordering = if (step.increasing) Ordering.Double else Ordering.Double.reverse
            byObsF andThen (obs =>
              obs.sortBy(_._2 match {
                case v: Int    => v.toDouble
                case v: Double => v
                case v: Long   => v.toDouble
              })(ordering))
          case lspace.NS.types.`@datetime` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: Instant), (l2, v2: Instant)) => v1.isBefore(v2)
                })
            } else {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: Instant), (l2, v2: Instant)) => v1.isAfter(v2)
                })
            }
          case lspace.NS.types.`@localdatetime` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: LocalDateTime), (l2, v2: LocalDateTime)) => v1.isBefore(v2)
                })
            } else {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: LocalDateTime), (l2, v2: LocalDateTime)) => v1.isAfter(v2)
                })
            }
          case lspace.NS.types.`@date` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: LocalDate), (l2, v2: LocalDate)) => v1.isBefore(v2)
                })
            } else {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: LocalDate), (l2, v2: LocalDate)) => v1.isAfter(v2)
                })
            }
          case lspace.NS.types.`@time` =>
            if (step.increasing) {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: LocalTime), (l2, v2: LocalTime)) => v1.isBefore(v2)
                })
            } else {
              byObsF andThen (obs =>
                obs.sortWith {
                  case ((l1, v1: LocalTime), (l2, v2: LocalTime)) => v1.isAfter(v2)
                })
            }
        }).andThen(_.map(_._1))
    }
    f andThen nextStep
  }

  def projectStep[Traversals <: HList](step: Project[Traversals], steps: List[Step], segments: List[Segment[_]])(
      implicit graph: Lspace): Stream[Librarian[Any]] => Stream[Any] = {
    val nextStep = buildNextStep(steps, segments)

    val pObs = step.by.runtimeList.map {
      case traversal: Traversal[ClassType[Any], ClassType[Any], HList] =>
        traversalToF(traversal.segmentList) -> traversal.steps.lastOption
    }

    val f = (obs: Stream[Librarian[Any]]) =>
      obs
        .map(librarian =>
          librarian -> pObs.map {
            case (pOb, Some(Count))                                        => pOb(librarian).head
            case (pOb, Some(step @ (_: Head | _: Min | _: Max | _: Mean))) => pOb(librarian).headOption
            case (pOb, Some(Last))                                         => pOb(librarian).lastOption
            case (pOb, _)                                                  => pOb(librarian).toList
        })
        .map {
          case (librarian, tuple) =>
            librarian.copy(tuple match {
              case List(v1)                     => v1
              case List(v1, v2)                 => (v1, v2)
              case List(v1, v2, v3)             => (v1, v2, v3)
              case List(v1, v2, v3, v4)         => (v1, v2, v3, v4)
              case List(v1, v2, v3, v4, v5)     => (v1, v2, v3, v4, v5)
              case List(v1, v2, v3, v4, v5, v6) => (v1, v2, v3, v4, v5, v6)
            })
      }

//    val f2 = step.by match {
//      case List(p1, p2) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        obs: Stream[Librarian[Any]] =>
//          obs.map { librarian =>
//            librarian.copy((p1Obs(librarian).toList, p2Obs(librarian).toList))
//          }
//      case List(p1, p2, p3) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        val p3Obs = traversalToF(p3.segmentList)
//        obs: Stream[Librarian[Any]] =>
//          obs.map { librarian =>
//            librarian.copy((p1Obs(librarian).toList, p2Obs(librarian).toList, p3Obs(librarian).toList))
//          }
//      case List(p1, p2, p3, p4) =>
//        val p1Obs = traversalToF(p1.segmentList)
//        val p2Obs = traversalToF(p2.segmentList)
//        val p3Obs = traversalToF(p3.segmentList)
//        val p4Obs = traversalToF(p4.segmentList)
//        obs: Stream[Librarian[Any]] =>
//          obs.map { librarian =>
//            librarian.copy(
//              (p1Obs(librarian).toList, p2Obs(librarian).toList, p3Obs(librarian).toList, p4Obs(librarian).toList))
//          }
//    }

    f andThen nextStep
  }

  protected def select(step: Select[_], traversers: Stream[Librarian[Any]])(
      implicit graph: Graph): Stream[Librarian[Any]] = {
    traversers.map { t =>
      step.names match {
        case List() =>
          t.copy(t.path.labeled.values.map(toValue) match {
            case List()           => t.get
            case List(a)          => a
            case List(a, b)       => (a, b)
            case List(a, b, c)    => (a, b, c)
            case List(a, b, c, d) => (a, b, c, d)
          })
        case List(a) =>
          val labeled = t.path.labeled.mapValues(toValue)
          t.copy(
            labeled
              .mapValues(toValue)
              .getOrElse(a, throw new Exception("could not select label 1 ...")))
        case List(a, b) =>
          val labeled = t.path.labeled.mapValues(toValue)
          t.copy(
            (labeled.getOrElse(a, throw new Exception("could not select label 1 ...")),
             labeled.getOrElse(b, throw new Exception("could not select label 2 ..."))))
        case List(a, b, c) =>
          val labeled = t.path.labeled.mapValues(toValue)
          t.copy(
            (labeled.getOrElse(a, throw new Exception("could not select label 1 ...")),
             labeled.getOrElse(b, throw new Exception("could not select label 2 ...")),
             labeled.getOrElse(c, throw new Exception("could not select label 3 ..."))))
        case List(a, b, c, d) =>
          val labeled = t.path.labeled.mapValues(toValue)
          t.copy(
            (labeled.getOrElse(a, throw new Exception("could not select label 1 ...")),
             labeled.getOrElse(b, throw new Exception("could not select label 2 ...")),
             labeled.getOrElse(c, throw new Exception("could not select label 3 ...")),
             labeled.getOrElse(d, throw new Exception("could not select label 4 ..."))))
      }
    }
  }
}
