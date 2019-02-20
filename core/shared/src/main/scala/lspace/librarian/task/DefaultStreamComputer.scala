package lspace.librarian.task

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.logic.Assistent

import scala.collection.mutable
import scala.util.Try
import lspace.librarian.logic.predicate.P
import lspace.librarian.traversal._
import lspace.librarian.task._
import lspace.librarian.traversal.step._
import lspace.structure._
import monix.reactive.Observable
import shapeless.{=:!=, HList, HNil, :: => ::::}

object DefaultStreamComputer {
  def apply(assistent: Assistent): DefaultStreamComputer = new DefaultStreamComputer(assistent)
}

class DefaultStreamComputer(val assistent: Assistent) extends Guide {

  implicit def segmentsToFlattenedSteps(segments: List[Segment[HList]]): List[Step] =
    segments.flatMap(_.stepsList)
  def traverse[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out, GT <: Graph](
      traversal: Traversal[ST, ET, Steps])(implicit
                                           graph: GT): Stream[Out] = {

    //TODO: scan/optimize traversal and pick an appropiate traverser, look for indexed patterns to start from

    traversal.segmentList.dropWhile(_.isInstanceOf[G]) match {
      case Nil => Stream.empty
      case segments =>
        segments.head.stepsList match {
          case Nil => Stream.empty
          case List(g: G) =>
            throw new Exception("GraphStep without follow up is an incomplete traversal")
          case steps =>
            Resourced
              .addSteps(steps ::: segments.tail.flatMap(_.stepsList))
              .map(_.get) //.asInstanceOf[Stream[End]]
              .map(toValue)
              .asInstanceOf[Stream[Out]]
        }
    }
  }

  def buildTraveralObservable[Out](segments: List[Segment[_]]): Graph => Observable[Out] = { graph: Graph =>
    Observable.fromIterable(segments.head.stepsList match {
      case Nil => Stream.empty
      case List(g: G) =>
        throw new Exception("GraphStep without follow up is an incomplete traversal")
      case steps =>
        Resourced
          .addSteps(steps ::: segments.tail.flatMap(_.stepsList))(graph)
          .map(_.get)
          .map(toValue)
          .asInstanceOf[Stream[Out]]
    })
  }

  private def is(step: Is, traversers: Stream[Librarian[Any]]): Stream[Librarian[Any]] = {
    traversers.filter { traverser =>
      assistent
        .pToHelper(step.predicate)
        .assert(traverser.get match {
          case r: Resource[Any] => r.value
          case v                => v
        })
    }
  }

  protected def addResourceStep[STEP <: ResourceStep, GT <: Graph](
      step: STEP,
      traverser: Option[Librarian[Any]] = None)(implicit graph: GT): Stream[Librarian[Resource[Any]]] = {
    implicit val implgraph = graph
    import graph._
    step match {
      //      case step: ResourceStep =>
      //        val streamIn = step.resources
      case step: N if step.nodes.forall {
            case node: Node => node.graph == this
          } =>
        step.nodes match {
          case List() =>
            nodes()
              .map(node => traverser.fold[Librarian[Any]](createLibrarian(node))(_.copy(get = node)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
          case list: List[Node] =>
            list.toStream
              .map(node => traverser.fold[Librarian[Any]](createLibrarian(node))(_.copy(get = node)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
        }
      case step: N =>
        nodes()
          .filter(step.nodes.contains)
          .map(node => traverser.fold[Librarian[Any]](createLibrarian(node))(_.copy(get = node)))
          .asInstanceOf[Stream[Librarian[Resource[Any]]]]
      case step: V if step.values.forall {
            case value: Value[Any] => value.graph == this
            case v                 => false
          } =>
        step.values match {
          case List() =>
            values()
              .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
          case list: List[Value[Any]] =>
            implgraph.values
              .byValue(list.map {
                case v: Value[Any] => v.value -> v.label
                case v             => v       -> ClassType.valueToOntologyResource(v)
              })
              .toStream
              .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
        }
      case step: V =>
        implgraph.values
          .byValue(step.values.map {
            case v: Value[Any] => v.value -> v.label
            case v             => v       -> ClassType.valueToOntologyResource(v)
          })
          .toStream
          .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
          .asInstanceOf[Stream[Librarian[Resource[Any]]]]
      case step: E if step.links.forall {
            case property: Edge[_, _] => property.graph == this
          } =>
        step.links match {
          case List() =>
            edges()
              .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
          case list: List[Edge[_, _]] =>
            list.toStream
              .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
        }
      case step: E =>
        edges()
          .filter(step.links.contains)
          .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
          .asInstanceOf[Stream[Librarian[Resource[Any]]]]
      case step: R if step.resources.forall {
            case value: Resource[Any] => value.graph == this
          } =>
        step.resources match {
          case List() =>
            (nodes() ++ edges() ++ values())
              .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
          case list: List[Resource[Any]] =>
            list.toStream
              .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
              .asInstanceOf[Stream[Librarian[Resource[Any]]]]
        }
      case step: R =>
        (nodes() ++ edges() ++ values())
          .filter(step.resources.contains)
          .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
          .asInstanceOf[Stream[Librarian[Resource[Any]]]]
      case _ =>
        nodes()
          .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
          .asInstanceOf[Stream[Librarian[Resource[Any]]]] ++
          edges()
            .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
            .asInstanceOf[Stream[Librarian[Resource[Any]]]] ++
          values()
            .map(v => traverser.fold[Librarian[Any]](createLibrarian(v))(_.copy(get = v)))
            .asInstanceOf[Stream[Librarian[Resource[Any]]]]
    }
  }

  object ResourceLess {
    protected[task] def addSteps(steps: List[Step], traversers: Stream[Librarian[Any]])(
        implicit graph: Graph): Stream[Librarian[Any]] = {
//      implicit val implgraph = graph

      steps.span {
        case _: BarrierStep | _: ClipStep | _: Project | _: ResourceStep | _: CollectingStep | _: MapStep | _: Id |
            _: Label | _: Select[_] =>
          false
        case _ => true
      } match {
        case (untilFilter, List()) =>
          untilFilter.foldLeft[Stream[Librarian[Any]]](traversers) {
            case (stream, step) => addStep(stream)(step)
          }
        case (List(), (rstep: ResourceStep) :: fromFilter) =>
          val newTraversers =
            traversers.flatMap(traverser => addResourceStep(rstep, Some(traverser)))
          Resourced.addSteps(fromFilter, Some(newTraversers))
        case (untilFilter, fromFilter) =>
          val untilFilterStream = untilFilter.foldLeft(traversers) {
            case (stream, step) => addStep(stream)(step)
          }
          fromFilter.head match {
            case step: ResourceStep =>
              Resourced.addSteps(fromFilter.tail,
                                 Some(untilFilterStream.flatMap(traverser => addResourceStep(step, Some(traverser)))))
            case step: Project => //TODO: shapeless list of project-by's and return tuple-x
              step.by match {
                case List(by1, by2) =>
                  untilFilterStream.map { traverser =>
                    traverser.copy(
                      (addSteps(by1.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList,
                       addSteps(by2.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList))
                  }
                case List(by1, by2, by3) =>
                  untilFilterStream.map { traverser =>
                    traverser.copy(
                      (addSteps(by1.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList,
                       addSteps(by2.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList,
                       addSteps(by3.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList))
                  }
                case List(by1, by2, by3, by4) =>
                  untilFilterStream.map { traverser =>
                    traverser.copy(
                      (addSteps(by1.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList,
                       addSteps(by2.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList,
                       addSteps(by3.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList,
                       addSteps(by4.segmentList, Stream(traverser))
                         .map(_.get)
                         .map(toValue)
                         .toList))
                  }
              }
            case step: Select[_] =>
              addSteps(fromFilter.tail, select(step, untilFilterStream))
          }
      }
    }

    protected[task] def addStep(traversers: Stream[Librarian[Any]])(step: Step)(
        implicit graph: Graph): Stream[Librarian[Any]] = {
      //    steps.foldLeft(traverser) {
      step match {
        case step: Coin => //TODO: deterministic flow of the traversal, different executions cannot produce different result sets for identical seeds, even for distributed execution
          traversers.filter { traverser =>
            Math.random() < step.p //get next seeded random value
          }
        case step: Is =>
          is(step, traversers)
        case step: As[_, _] =>
          traversers.map { t =>
            val labeled = t.path.labeled + (step.label -> t.get)
            val path    = t.path.copy(labeled = labeled)
            t.copy(path = path)
          }
      }
    }
  }

  object Resourced {
    protected[task] def addSteps(steps: List[Step], traversers: Option[Stream[Librarian[Resource[Any]]]] = None)(
        implicit graph: Graph): Stream[Librarian[Any]] = {
//      implicit val implgraph = graph
      import graph.{assistent => _, _}

      if (steps.isEmpty) traversers.getOrElse(Stream[Librarian[Resource[Any]]]())
      else
        steps.span {
          case _: BarrierStep | _: ClipStep | _: Project | _: ResourceStep | _: CollectingStep | _: MapStep | _: Id |
              _: Label | _: Select[_] =>
            false
          case _ => true
        } match {
          case (untilFilter, List()) =>
            untilFilter.foldLeft[Stream[Librarian[Resource[Any]]]](
              traversers
                .getOrElse(
                  nodes().map(v => createLibrarian(v)) ++
                    edges().map(v => createLibrarian(v)) ++
                    values().map(v => createLibrarian(v)))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]) {
              case (stream, step) => addStep(stream)(step)
            }
          case (List(), (rstep: ResourceStep) :: fromFilter) =>
            val newTraversers = traversers.fold(addResourceStep(rstep)) { traversers =>
              traversers.flatMap(traverser => addResourceStep(rstep, Some(traverser)))
            }
            addSteps(fromFilter, Some(newTraversers))
          case (untilFilter, fromFilter) =>
            val untilFilterStream = untilFilter.foldLeft(
              traversers
                .getOrElse(
                  nodes().map(v => createLibrarian(v)) ++
                    edges().map(v => createLibrarian(v)) ++
                    values().map(v => createLibrarian(v)))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]) {
              case (stream, step) => addStep(stream)(step)
            }
            fromFilter.head match {
              case step: ResourceStep =>
                addSteps(fromFilter.tail,
                         Some(untilFilterStream.flatMap(traverser => addResourceStep(step, Some(traverser)))))
              case step: Id =>
                ResourceLess.addSteps(fromFilter.tail,
                                      untilFilterStream.map(traverser => traverser.copy(traverser.get.id)))
              case step: Label =>
                val labelStream =
                  if (step.label.isEmpty)
                    untilFilterStream.flatMap(traverser =>
                      traverser.get.labels.toStream.map(r =>
                        traverser.copy(r /*, path = traverser.path.copy(traverser.path.resources :+ r)*/ )))
                  else
                    untilFilterStream
                      .flatMap(traverser =>
                        traverser.get.labels.toStream.map(r =>
                          traverser.copy(r /*, path = traverser.path.copy(traverser.path.resources :+ r)*/ )))
                      .filter(t => step.label.contains(t.get))
                fromFilter.tail match {
                  case Nil => labelStream
                  case step :: tail =>
                    step match {
                      case step: Is =>
                        is(step, labelStream)
                    }
                }
              case step: Project => //TODO: shapeless list of project-by's and return tuple-x
                step.by match {
                  case List(by1, by2) =>
                    untilFilterStream.map { traverser =>
                      traverser.copy(
                        (addSteps(by1.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList,
                         addSteps(by2.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList))
                    }
                  case List(by1, by2, by3) =>
                    untilFilterStream.map { traverser =>
                      traverser.copy(
                        (addSteps(by1.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList,
                         addSteps(by2.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList,
                         addSteps(by3.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList))
                    }
                  case List(by1, by2, by3, by4) =>
                    untilFilterStream.map { traverser =>
                      traverser.copy(
                        (addSteps(by1.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList,
                         addSteps(by2.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList,
                         addSteps(by3.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList,
                         addSteps(by4.segmentList, Some(Stream(traverser)))
                           .map(_.get)
                           .map(toValue)
                           .toList))
                    }
                }
              case step: Select[_] =>
//                addSteps(fromFilter.tail, select(step, untilFilterStream))
                val selectStream = select(step, untilFilterStream)
                fromFilter.tail match {
                  case Nil => selectStream
                  case step :: tail =>
                    step match {
                      case step: Is =>
                        is(step, selectStream)
                    }
                }
              case step: BarrierStep =>
                step match {
                  case step: FilterBarrierStep =>
                    val reducedTraverser = step match {
                      case step: Max => task.Max(untilFilterStream)
                      case step: Min => task.Min(untilFilterStream)
                    }
                    fromFilter.tail match {
                      case Nil => reducedTraverser
                      case step :: tail =>
                        step match {
                          case step: Is =>
                            is(step, reducedTraverser)
                          case step =>
                            addSteps(fromFilter.tail, Some(reducedTraverser))
                        }
                    }
                  case step: ReducingBarrierStep =>
                    step match {
                      case step: Count =>
                        //                    println(s"counting ${untilFilterStream.size.toLong} traversers")
                        val reducedTraverser = createLibrarian(untilFilterStream.size.toLong)
                        fromFilter.tail match {
                          case Nil => Stream(reducedTraverser)
                          case step :: tail =>
                            step match {
                              case step: Is =>
                                is(step, Stream(reducedTraverser))
                            }
                        }
                      case step: Mean =>
                        //                    println(s"counting ${untilFilterStream.size.toLong} traversers")
                        val reducedTraverser = createLibrarian(task.Mean(untilFilterStream.map(_.get)))
                        fromFilter.tail match {
                          case Nil => Stream(reducedTraverser)
                          case step :: tail =>
                            step match {
                              case step: Is =>
                                is(step, Stream(reducedTraverser))
                            }
                        }
                      case step: Sum =>
                        //                    println(s"counting ${untilFilterStream.size.toLong} traversers")
                        val reducedTraverser = createLibrarian(task.Sum(untilFilterStream.map(_.get)))
                        fromFilter.tail match {
                          case Nil => Stream(reducedTraverser)
                          case step :: tail =>
                            step match {
                              case step: Is =>
                                is(step, Stream(reducedTraverser))
                            }
                        }
                      //        case step: Fold => Traverser(stream.map(_.get).toList, dataType = ???)
                    }
                  case step: RearrangeBarrierStep =>
                    step match {
                      case step: Order =>
//                        val helper: P.OrderHelper[Any] = P.OrderHelper.get(step.by.et)
                        val until = untilFilterStream
                          .flatMap(
                            traverser =>
                              addSteps(step.by.segmentList, Some(Stream(traverser))).headOption
                                .map(v => v -> traverser))
                          .map { t =>
                            t._1.get match {
                              case resource: Resource[Any] => resource.value -> t._2
                              case v: Any                  => v              -> t._2
                            }
                          }
                        val orderedStream = (step.by.et.iri match {
                          case lspace.NS.types.`@int` =>
                            until.sortWith { case ((v1: Int, t1), (v2: Any, t2)) => assistent.orderhelper.int(v1, v2) }
                          case lspace.NS.types.`@double` =>
                            until.sortWith {
                              case ((v1: Double, t1), (v2: Any, t2)) => assistent.orderhelper.double(v1, v2)
                            }
                          case lspace.NS.types.`@long` =>
                            until.sortWith {
                              case ((v1: Long, t1), (v2: Any, t2)) => assistent.orderhelper.long(v1, v2)
                            }
                          case lspace.NS.types.`@number` =>
                            until.sortWith {
                              case ((v1: Int, t1), (v2: Int, t2))       => assistent.orderhelper.double(v1, v2)
                              case ((v1: Int, t1), (v2: Double, t2))    => assistent.orderhelper.double(v1, v2)
                              case ((v1: Int, t1), (v2: Long, t2))      => assistent.orderhelper.double(v1, v2)
                              case ((v1: Double, t1), (v2: Int, t2))    => assistent.orderhelper.double(v1, v2)
                              case ((v1: Double, t1), (v2: Double, t2)) => assistent.orderhelper.double(v1, v2)
                              case ((v1: Double, t1), (v2: Long, t2))   => assistent.orderhelper.double(v1, v2)
                              case ((v1: Long, t1), (v2: Int, t2))      => assistent.orderhelper.double(v1, v2)
                              case ((v1: Long, t1), (v2: Double, t2))   => assistent.orderhelper.double(v1, v2)
                              case ((v1: Long, t1), (v2: Long, t2))     => assistent.orderhelper.double(v1, v2)
                              case _                                    => false
                            }
                          case lspace.NS.types.`@datetime` =>
                            until.sortWith {
                              case ((v1: Instant, t1), (v2: Any, t2)) => assistent.orderhelper.datetime(v1, v2)
                            }
                          case lspace.NS.types.`@localdatetime` =>
                            until.sortWith {
                              case ((v1: LocalDateTime, t1), (v2: Any, t2)) =>
                                assistent.orderhelper.localdatetime(v1, v2)
                            }
                          case lspace.NS.types.`@date` =>
                            until.sortWith {
                              case ((v1: LocalDate, t1), (v2: Any, t2)) => assistent.orderhelper.localdate(v1, v2)
                            }
                          case lspace.NS.types.`@time` =>
                            until.sortWith {
                              case ((v1: LocalTime, t1), (v2: Any, t2)) => assistent.orderhelper.localtime(v1, v2)
                            }
                        }).map(_._2)
                        //                    val orderedStream = untilFilterStream.head.get.value match {
                        //                      case _: Int => untilFilterStream.sortBy(_.get.value.asInstanceOf[Int])
                        //                      case _: Double => untilFilterStream.sortBy(_.get.value.asInstanceOf[Double])
                        //                      case _: Long => untilFilterStream.sortBy(_.get.value.asInstanceOf[Long])
                        //                    }
                        addSteps(fromFilter.tail, Some(if (step.increasing) orderedStream else orderedStream.reverse))
                    }
                  case step: CollectingBarrierStep =>
                    step match {
                      case step: Group[_] =>
                        val groupedStreams = untilFilterStream
                          .map { t =>
                            addSteps(step.by.segmentList, Some(Stream(t)))
                              .map(_.get match {
                                case resource: Resource[Any] => resource.value
                                case v: Any                  => v
                              })
                              .toList -> t
                          }
                          .groupBy(_._1)
                          .mapValues(_.map(_._2))
                        Stream(
                          createLibrarian(
                            groupedStreams
                              .mapValues { stream =>
                                addSteps(fromFilter.tail, Some(stream))
                                  .map(_.get)
                                  .map(toValue)
                                  .toList
                              }
                              .filterNot(_._2.isEmpty)))
                    }
                }
              case step: MapStep =>
                step match {
                  case step: OutMap =>
                    untilFilterStream.map { t =>
                      t.copy(
                        t.get
                          .outEMap(step.label.toList: _*)
                          .mapValues(_.map(_.to))
                          .map {
                            case (property, values) =>
                              property -> addSteps(
                                fromFilter.tail,
                                Some(values.toStream.asInstanceOf[Stream[Resource[Any]]].map(v => t.copy(v))))
                                .map(_.get)
                                .map(toValue)
                                .toList
                          })
                    }
                  case step: OutEMap =>
                    untilFilterStream.map { t =>
                      t.copy(t.get.outEMap(step.label.toList: _*).map {
                        case (property, values) =>
                          property -> addSteps(
                            fromFilter.tail,
                            Some(values.toStream.asInstanceOf[Stream[Resource[Any]]].map(v => t.copy(v))))
                            .map(_.get)
                            .map(toValue)
                            .toList
                      })
                    }
                  case step: InMap =>
                    untilFilterStream.map { t =>
                      t.copy(t.get.inEMap(step.label.toList: _*).mapValues(_.map(_.from)).map {
                        case (property, values) =>
                          property -> addSteps(
                            fromFilter.tail,
                            Some(values.toStream.asInstanceOf[Stream[Resource[Any]]].map(v => t.copy(v))))
                            .map(_.get)
                            .map(toValue)
                            .toList
                      })
                    }
                  case step: InEMap =>
                    untilFilterStream.map { t =>
                      t.copy(t.get.inEMap(step.label.toList: _*).map {
                        case (property, values) =>
                          property -> addSteps(
                            fromFilter.tail,
                            Some(values.toStream.asInstanceOf[Stream[Resource[Any]]].map(v => t.copy(v))))
                            .map(_.get)
                            .map(toValue)
                            .toList
                      })
                    }
                  case step: Path =>
                    untilFilterStream.map { traverser =>
                      traverser.copy(
                        traverser.path.resources.map(
                          r =>
                            addSteps(step.by.segmentList, Some(Stream(createLibrarian(r.asInstanceOf[Resource[Any]]))))
                              .map(_.get)
                              .map(toValue)
                              .toList))
                    }
                }
              case step: ClipStep =>
                val clippedStream = step match {
                  case step: Range => untilFilterStream.slice(step.low, step.high)
                  case step: Limit => untilFilterStream.take(step.max)
                  case step: Tail  => untilFilterStream.takeRight(step.max)
                }
                addSteps(fromFilter.tail, Some(clippedStream))
            }
        }
    }

    protected[task] def addStep(traversers: Stream[Librarian[Resource[Any]]])(step: Step)(
        implicit graph: Graph): Stream[Librarian[Resource[Any]]] = {
      //    steps.foldLeft(traverser) {
      step match {
        case step: MoveStep =>
          step match {
            case step: Out =>
              traversers
                .flatMap(
                  traverser =>
                    traverser.get
                      .outE(step.label.toList: _*)
                      .toStream
                      .map(r => traverser.copy(r.inV, path = traverser.path.copy(traverser.path.resources :+ r))))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]
            case step: OutE =>
              traversers.flatMap(
                traverser =>
                  traverser.get
                    .outE(step.label.toList: _*)
                    .toStream
                    .map(r =>
                      traverser
                        .copy(r, path = traverser.path.copy(traverser.path.resources :+ r)))
                    .asInstanceOf[Stream[Librarian[Resource[Any]]]])
            case step: In =>
              traversers
                .flatMap(
                  traverser =>
                    traverser.get
                      .inE(step.label.toList: _*)
                      .toStream
                      .map(r => traverser.copy(r.outV, path = traverser.path.copy(traverser.path.resources :+ r))))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]
            case step: InE =>
              traversers.flatMap(
                traverser =>
                  traverser.get
                    .inE(step.label.toList: _*)
                    .toStream
                    .map(r =>
                      traverser
                        .copy(r, path = traverser.path.copy(traverser.path.resources :+ r)))
                    .asInstanceOf[Stream[Librarian[Resource[Any]]]])
          }
        case step: FilterStep =>
          step match {
            case step: Has =>
              val helper = step.predicate.map(assistent.pToHelper)
              traversers.filter { traverser =>
                val toAssert = traverser.get.out(step.key) //.collect { case edge if step.range.headOption.isDefined => edge.inV.hasLabel(step.range.head) }.flatten
                if (toAssert.isEmpty) false
                else
                  helper
                    .forall(p => toAssert.exists(v => p.comparable(v) && p.assert(v)))
              }
            case step: HasNot =>
              val helper = step.predicate.map(assistent.pToHelper)
              traversers.filterNot { traverser =>
                val toAssert = traverser.get.out(step.key) //.collect { case edge if step.range.headOption.isDefined => edge.inV.hasLabel(step.range.head) }.flatten
                if (toAssert.isEmpty) false
                else
                  helper
                    .exists(p => toAssert.exists(v => p.comparable(v) && p.assert(v)))
              }
            case step: Not =>
              traversers.filter(traverser => addSteps(step.traversal.segmentList, Some(Stream(traverser))).isEmpty)
            case step: HasId =>
              val longIds = step.ids.flatMap(id => Try { id.toLong }.toOption)
              traversers.filter(traverser => longIds.contains(traverser.get.id))
            case step: HasIri =>
              traversers.filter(traverser => traverser.get.iris.intersect(step.iris).nonEmpty)
            case step: HasLabel =>
              val labelIris       = step.label.map(_.iri)
              val labelClassTypes = labelIris.flatMap(graph.ns.classtypes.cached(_))
              traversers.filter { traverser =>
//                traversers.map(_.get.value)
                traverser.get match {
                  case node: Node =>
                    //                  step.label.map(_.iri).toStream.intersect(node.labels.map(_.iri)).nonEmpty
                    node.labels.exists {
                      case label if labelIris.contains(label.iri)                   => true
                      case label if labelClassTypes.exists(l => label.`extends`(l)) => true
                      case _                                                        => false
                    }
                  case property: Edge[_, _] =>
                    //                println(s"has edge label ${step.label.map(_.iri)} =?= ${property.labels.map(_.iri)}")
                    //                  step.label.map(_.iri).contains(property.key.iri)
                    property.key match {
                      case label if labelIris.contains(label.iri)                   => true
                      case label if labelClassTypes.exists(l => label.`extends`(l)) => true
                      case _                                                        => false
                    }
                  case value: Value[_] =>
                    //                step.label.contains(value.dataType)
                    //                  step.label.map(_.iri).contains(value.label.iri)
                    value.label match {
                      case label if labelIris.contains(label.iri)                   => true
                      case label if labelClassTypes.exists(l => label.`extends`(l)) => true
                      case _                                                        => false
                    }
                  case _ => false
                }
              }
            case step: HasValue =>
              val helper = assistent.pToHelper(step.predicate)
              traversers.filter { traverser =>
                helper.comparable(traverser.get.value) && helper.assert(traverser.get.value)
              //            traverser.get match {
              //              case value: Value[Any] =>
              //                step.predicate.asInstanceOf[List[P[Any]]]
              //                  .forall(_.assert(traverser.get.value))
              //              case r =>
              //                step.predicate.asInstanceOf[List[P[Any]]]
              //                  .forall(_.assert(r.iri))
              //            }
              }
            case step: Dedup =>
              val results = mutable.HashSet[Any]()
              traversers.filter { t =>
                if (results.contains(t.get.value)) false
                else {
                  results += t.get.value
                  true
                }
              }
            case step: Coin => //TODO: deterministic flow of the traversal, different executions cannot produce different result sets for identical seeds, even for distributed execution
              traversers.filter { traverser =>
                Math.random() < step.p //get next seeded random value
              }
            case step: Is =>
              is(step, traversers).asInstanceOf[Stream[Librarian[Resource[Any]]]]
            case step: Where =>
              traversers.filter(traverser => addSteps(step.traversal.segmentList, Some(Stream(traverser))).nonEmpty)
            case step: And =>
              traversers.filter(traverser =>
                step.traversals.forall(traversal => addSteps(traversal.segmentList, Some(Stream(traverser))).nonEmpty))
            case step: Or =>
              traversers.filter(traverser =>
                step.traversals.exists(traversal => addSteps(traversal.segmentList, Some(Stream(traverser))).nonEmpty))
          }
        case step: BranchStep =>
          step match {
            case step: Union[_, _] =>
              traversers
                .flatMap(traverser =>
                  step.traversals.flatMap(traversal => addSteps(traversal.segmentList, Some(Stream(traverser)))))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]
            case step: Coalesce[_, _] =>
              traversers
                .flatMap(
                  traverser =>
                    step.traversals.toStream
                      .map(traversal => addSteps(traversal.segmentList, Some(Stream(traverser))))
                      .collectFirst { case result if result.nonEmpty => result }
                      .getOrElse(Stream()))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]
            case step: Local =>
              traversers
                .flatMap(traverser => addSteps(step.traversal.segmentList, Some(Stream(traverser))))
                .asInstanceOf[Stream[Librarian[Resource[Any]]]]
            case step: Repeat[_] =>
              traversers.flatMap(traverser => repeat(step, traverser))
          }
        case step: As[_, _] =>
          traversers.map(t => t.copy(path = t.path.copy(labeled = t.path.labeled + (step.label -> t.get))))
        case step: Drop =>
          //        traversers.foreach(_.get.remove()) //TODO create computer options to enable editing the graph
          Stream()

        //          case step: Is[_] => if(step.predicate.assert(List(traverser.get.value))) Stream(traverser.asInstanceOf[Traverser[MemResource[End]]]) else Stream()
      }
    }

    protected def repeat(step: Repeat[_], traverser: Librarian[Resource[Any]], repeats: Int = 0)(
        implicit graph: Graph): Stream[Librarian[Resource[Any]]] = {
      val collect = step.collect.getOrElse(false)
      if (repeats > 30) Stream(traverser) //TODO: how to handle possible infinite loops
      else
        addSteps(step.traversal.segmentList, Some(Stream(traverser)))
          .asInstanceOf[Stream[Librarian[Resource[Any]]]]
          .flatMap { traverser =>
            step.until match {
              case Some(until) =>
                if (addSteps(until.segmentList, Some(Stream(traverser))).isEmpty) {
                  step.max match {
                    case Some(max) =>
                      if (repeats + 1 < max) {
                        if (collect) Stream(traverser) ++ repeat(step, traverser, repeats + 1)
                        else repeat(step, traverser, repeats + 1)
                      } else Stream(traverser)
                    case None =>
                      if (repeats > 20) 1 //TODO warn for infinite loop
                      if (collect) Stream(traverser) ++ repeat(step, traverser, repeats + 1)
                      else repeat(step, traverser, repeats + 1)
                  }
                } else Stream(traverser)
              case None =>
                step.max match {
                  case Some(max) =>
                    if (repeats + 1 < max) {
                      if (collect) Stream(traverser) ++ repeat(step, traverser, repeats + 1)
                      else repeat(step, traverser, repeats + 1)
                    } else Stream(traverser)
                  case None =>
                    if (repeats > 20) 1 //TODO warn for infinite loop
                    if (collect) Stream(traverser) ++ repeat(step, traverser, repeats + 1)
                    else repeat(step, traverser, repeats + 1)
                }
            }
          }
    }
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
