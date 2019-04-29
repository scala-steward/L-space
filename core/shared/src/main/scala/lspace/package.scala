import lspace.ExecutionHelper
import lspace.librarian.logic.{Assistent, DefaultAssistent}
import lspace.librarian.task
import lspace.structure.{ClassType, Edge, Node, Resource, Value}
import lspace.structure.util.ClassTypeable
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import shapeless.HList

package object lspace {

  type Graph = structure.Graph
  val Graph = structure.Graph
  type Node = structure.Node
  val Node = structure.Node
  type Edge[+S, +E] = structure.Edge[S, E]
  val Edge = structure.Edge
  type Value[+T] = structure.Value[T]
  val Value = structure.Value
  type Resource[+T] = structure.Resource[T]
  val Resource = structure.Resource
  type ClassType[+T] = structure.ClassType[T]
  val ClassType = structure.ClassType
  type Ontology = structure.Ontology
  val Ontology = structure.Ontology
  type Property = structure.Property
  val Property = structure.Property
  type TypedProperty[T] = structure.TypedProperty[T]
  val TypedProperty = structure.TypedProperty
  type DataType[+T] = datatype.DataType[T]
  val DataType = datatype.DataType

  type P[Z] = lspace.librarian.logic.predicate.P[Z]
  val P = lspace.librarian.logic.predicate.P

  type Traversal[+ST <: ClassType[_], +ET <: ClassType[_], Steps <: HList] =
    lspace.librarian.traversal.Traversal[ST, ET, Steps]
//  implicit def tToT[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList](
//      t: Traversal[ST, ET, Steps]): lspace.librarian.traversal.Traversal[ST, ET, Steps] = t

  val Traversal                                      = lspace.librarian.traversal.Traversal
  def g                                              = Traversal()
  def __[Start: ClassTypeable, End: ClassTypeable]() = Traversal[Start, End]

  val Label = new {
    val P = structure.Property.default
    val D = datatype.DataType.default
  }
  val Properties = structure.Property.default
  val DataTypes  = datatype.DataType.default

  object Implicits {
    object DefaultAssistent {
      implicit val assistent: Assistent = librarian.logic.DefaultAssistent()
    }
    object AsyncGuide {
      implicit val guide: task.Guide[Observable] = task.AsyncGuide()(DefaultAssistent.assistent)
    }
    object SyncGuide {
      implicit val guide: task.Guide[Stream] = task.SyncGuide()(DefaultAssistent.assistent)
    }

    object Scheduler {
      implicit def global: Scheduler = ExecutionHelper.scheduler
    }
  }

  abstract class ResourceTask[T, R[_] <: Resource[_]](value0: Task[R[T]]) {
    def id: Task[Long] = value0.map(_.id)
    def value: Task[T] = value0.map(_.value.asInstanceOf[T])
  }
  abstract class ResourceOptionTask[T, R[_] <: Resource[_]](value0: Task[Option[R[T]]]) {
    def id: Task[Option[Long]] = value0.map(_.map(_.id))
    def value: Task[Option[T]] = value0.map(_.map(_.value.asInstanceOf[T]))
  }
  implicit class WithTaskNode(value: Task[Node])               extends ResourceTask[Node, Resource](value)
  implicit class WithTaskNodeOption(value: Task[Option[Node]]) extends ResourceOptionTask[Node, Resource](value)
  implicit class WithTaskEdge(value: Task[Edge[_, _]])         extends ResourceTask[Edge[_, _], Resource](value)
  implicit class WithTaskEdgeOption(value: Task[Option[Edge[_, _]]])
      extends ResourceOptionTask[Edge[_, _], Resource](value)
  implicit class WithTaskValue[T](value: Task[Value[T]])               extends ResourceTask[T, Value](value)
  implicit class WithTaskValueOption[T](value: Task[Option[Value[T]]]) extends ResourceOptionTask[T, Value](value)
  implicit class WithTaskResource[T](value: Task[Resource[T]])         extends ResourceTask[T, Resource](value)
  implicit class WithTaskResourceOption[T](value: Task[Option[Resource[T]]])
      extends ResourceOptionTask[T, Resource](value)

}
