package lspace.librarian.task

import java.time.Instant

import cats.Functor
import cats.implicits._
import lspace.librarian.traversal.step._
import lspace.librarian.traversal.util.EndMapper
import lspace.librarian.traversal.{Librarian, Traversal, TraversalPath}
import lspace.structure.{ClassType, Graph, Resource}
import shapeless.HList

import scala.concurrent.duration.FiniteDuration

abstract class Guide[F[_]: Functor] {

  type K[_]
//  implicit def kFunctor: Functor[F]

  def executeTraversal[Out](
    traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]
  ): Graph => F[Out] = { graph =>
    graph.traverse(traversal, this).asInstanceOf[F[Out]]
  }

//  implicit class WithF[A](list: F[A]) {
//    def map[B](f: A => B)(implicit func: Functor[F]): F[B] = func.map(list)(f)

  def emptyF[T]: F[T]
  def createF[T](t: T): F[T]
  def createLibrarian[T](
    get: T,
    path: TraversalPath = TraversalPath(),
    loops: Int = 0,
    mit: Option[Instant] = None,
    permissions: List[String] = List()
  ): Librarian[T] =
    SimpleLibrarian[T](get)(path, loops, mit, permissions)

  protected def head(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def headOption(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def headOptionOption(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def toList(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def toSet(f: F[Librarian[Any]]): K[Librarian[Any]]
  protected def toMap(f: F[Librarian[(Any, Any)]]): K[Librarian[Any]]
  protected def takeByTimeSpan(f: F[Librarian[Any]], timespan: FiniteDuration): F[Librarian[Any]]

  def raiseError[T](ex: Exception): F[T]

  def buildTraversal[Out](
    traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]
  ): Graph => F[Out] = { implicit graph: Graph =>
    traversal.stepsList match {
      case Nil => emptyF[Out].asInstanceOf[F[Any]]
      case steps =>
        createF(1)
        val nextStep = buildNextStep(steps)
        nextStep(createF(createLibrarian[Any](null))).asInstanceOf[F[Any]]
    }
  }.andThen(_.map(toValue).asInstanceOf[F[Out]])

  def buildNextStep(steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]]

  def traversalToF(
    traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]
  )(implicit graph: Graph): Librarian[Any] => F[Librarian[Any]] =
    traversal.stepsList match {
      case Nil =>
        (librarian: Librarian[Any]) => createF(librarian)
      case steps =>
        val nextStep = buildNextStep(steps)
        (librarian: Librarian[Any]) => nextStep(createF(librarian))
    }

  def traversalsToF(
    traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]
  )(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]] =
    traversal.stepsList match {
      case Nil =>
        (librarians: F[Librarian[Any]]) => librarians
      case steps =>
        val nextStep = buildNextStep(steps)
        (librarians: F[Librarian[Any]]) => nextStep(librarians)
    }

  def reducedEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
    if (EndMapper.EndMapper0.ReducedEnd.is(steps)) Some { (observable: F[Librarian[Any]]) =>
      headOption(observable)
    }
    else None

  def singularEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
    if (EndMapper.EndMapper0.SingularEnd.is(steps)) Some { (observable: F[Librarian[Any]]) =>
      head(observable)
    }
//    else if (EndMapper.EndMapper0.SingularFilteredEnd.is(steps)) Some({ observable: F[Librarian[Any]] =>
//      headOption(observable)
//    })
    else None

  def filteredEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
    if (EndMapper.EndMapper0.FilteredEnd.is(steps)) Some { (observable: F[Librarian[Any]]) =>
      headOption(observable)
    }
    else None

//  def branchedEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
//    if (EndMapper.EndMapper0.BranchedEnd.is(steps)) Some({ observable: F[Librarian[Any]] =>
//      toList(observable)
//    })
//    else None
//
//  def resourcedEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
//    if (EndMapper.EndMapper0.ResourcedEnd.is(steps)) Some({ observable: F[Librarian[Any]] =>
//      toList(observable)
//    })
//    else None

  def distinctedEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
    if (EndMapper.EndMapper0.DistinctedEnd.is(steps)) Some { (observable: F[Librarian[Any]]) =>
      toSet(observable)
    }
    else None

  def groupedEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
    if (EndMapper.EndMapper0.GroupedEnd.is(steps)) Some { (observable: F[Librarian[Any]]) =>
      toMap(observable.asInstanceOf[F[Librarian[(Any, Any)]]])
    }
    else None

  def oneOnOneEnd(steps: List[Step]): Option[F[Librarian[Any]] => K[Librarian[Any]]] =
    if (EndMapper.EndMapper0.OneOnOneEnd.is(steps)) Some { (observable: F[Librarian[Any]]) =>
      head(observable)
    }
    else None

  //transform a nested traversal result to it's expected type
  def endMapper(traversal: Traversal[ClassType[Any], ClassType[Any], HList]): F[Librarian[Any]] => K[Librarian[Any]] = {
//    branchedEnd(traversal.stepsList)
//      .orElse(resourcedEnd(traversal.stepsList))
//      .orElse(
    val steps = traversal.stepsList.reverse
    reducedEnd(steps)
      .orElse(singularEnd(steps))
      .orElse(filteredEnd(steps))
      .orElse(distinctedEnd(steps))
      .orElse(groupedEnd(steps))
      .orElse(oneOnOneEnd(steps))
      .getOrElse { (observable: F[Librarian[Any]]) =>
        toList(observable)
      }
  }

  def toValue(v: Any): Any = v match {
    case librarian: Librarian[_] => toValue(librarian.get)
    case Some(value)             => Some(toValue(value))
    case resource: Resource[_]   => resource.value
    case it: Map[_, _]           => it.map(t => toValue(t._1) -> toValue(t._2))
    case it: Iterable[_]         => it.map(toValue)
    case product: Product =>
      product match {
        case (v1, v2)                 => (toValue(v1), toValue(v2))
        case (v1, v2, v3)             => (toValue(v1), toValue(v2), toValue(v3))
        case (v1, v2, v3, v4)         => (toValue(v1), toValue(v2), toValue(v3), toValue(v4))
        case (v1, v2, v3, v4, v5)     => (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5))
        case (v1, v2, v3, v4, v5, v6) => (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5), toValue(v6))
        case (v1, v2, v3, v4, v5, v6, v7) =>
          (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5), toValue(v6), toValue(v7))
        case (v1, v2, v3, v4, v5, v6, v7, v8) =>
          (toValue(v1), toValue(v2), toValue(v3), toValue(v4), toValue(v5), toValue(v6), toValue(v7), toValue(v8))
        case (v1, v2, v3, v4, v5, v6, v7, v8, v9) =>
          (
            toValue(v1),
            toValue(v2),
            toValue(v3),
            toValue(v4),
            toValue(v5),
            toValue(v6),
            toValue(v7),
            toValue(v8),
            toValue(v9)
          )
        case value => value
      }
    case value => value
  }

  def labelStep(step: LabelStep, steps: List[Step])(implicit graph: Graph): F[Librarian[Any]] => F[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step match {
      case step: As[_, _] =>
        (obs: F[Librarian[Any]]) => obs.map { l => l.copy(path = l.path.copy(labeled = l.path.labeled + (step.label -> l.get)))}
      case _ => throw new Exception(s"invalid type ${step.getClass.getSimpleName}")
    }
    f.andThen { r =>
      r.map {
        case l: Librarian[Any] => l
        case other             => createLibrarian(other)
      }
    }.andThen(nextStep)
  }

  def selectStep(step: Select[_], steps: List[Step])(implicit
    graph: Graph
  ): F[Librarian[Any]] => F[Librarian[Any]] = {
    val nextStep = buildNextStep(steps)

    val f = step.names match {
      case Nil =>
        (obs: F[Librarian[Any]]) =>
          obs.map { l =>
            l.copy(l.path.labeled.values.map(toValue) match {
              case List()           => l.get
              case List(a)          => a
              case List(a, b)       => (a, b)
              case List(a, b, c)    => (a, b, c)
              case List(a, b, c, d) => (a, b, c, d)
              case v => throw new Exception(s"invalid type ${v.getClass.getSimpleName}")
            })
          }
      case List(a) =>
        (obs: F[Librarian[Any]]) =>
          obs.map { l =>
            l.copy(
              l.path.labeled
                .get(a)
                .getOrElse(a, throw new Exception("could not select label 1 ..."))
            )
          }
      case List(a, b) =>
        (obs: F[Librarian[Any]]) =>
          obs.map { l =>
            l.copy(
              (
                l.path.labeled.get(a).getOrElse(a, throw new Exception("could not select label 1 ...")),
                l.path.labeled.get(b).getOrElse(b, throw new Exception("could not select label 2 ..."))
              )
            )
          }
      case List(a, b, c) =>
        (obs: F[Librarian[Any]]) =>
          obs.map { l =>
            l.copy(
              (
                l.path.labeled.get(a).getOrElse(a, throw new Exception("could not select label 1 ...")),
                l.path.labeled.get(b).getOrElse(b, throw new Exception("could not select label 2 ...")),
                l.path.labeled.get(c).getOrElse(c, throw new Exception("could not select label 3 ..."))
              )
            )
          }
      case List(a, b, c, d) =>
        (obs: F[Librarian[Any]]) =>
          obs.map { l =>
            l.copy(
              (
                l.path.labeled.get(a).getOrElse(a, throw new Exception("could not select label 1 ...")),
                l.path.labeled.get(b).getOrElse(b, throw new Exception("could not select label 2 ...")),
                l.path.labeled.get(c).getOrElse(c, throw new Exception("could not select label 3 ...")),
                l.path.labeled.get(d).getOrElse(d, throw new Exception("could not select label 4 ..."))
              )
            )
          }
      case _ => throw new Exception(s"can not select ${step.names.size} labels (yet), max is 4 currently")
    }

    f.andThen { r =>
      r.map {
        case l: Librarian[Any] => l
        case other             => createLibrarian(other)
      }
    }.andThen(nextStep)
  }
}
