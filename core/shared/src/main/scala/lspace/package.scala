import lspace.librarian.logic.{Assistent, DefaultAssistent}
import lspace.librarian.task
import lspace.structure.ClassType
import lspace.structure.util.ClassTypeable
import monix.reactive.Observable
import shapeless.HList

package object lspace {

  type Graph = structure.Graph

  type P[Z] = lspace.librarian.logic.predicate.P[Z]
  val P = lspace.librarian.logic.predicate.P

  type Traversal[+ST <: ClassType[_], +ET <: ClassType[_], Steps <: HList] =
    lspace.librarian.traversal.Traversal[ST, ET, Steps]
  implicit def tToT[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList](
      t: Traversal[ST, ET, Steps]): lspace.librarian.traversal.Traversal[ST, ET, Steps] = t

  val Traversal                                      = lspace.librarian.traversal.Traversal
  def g                                              = Traversal()
  def __[Start: ClassTypeable, End: ClassTypeable]() = Traversal[Start, End]

  val Label = new {
    val P = structure.Property.default
    val D = datatype.DataType.default
  }

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
  }
}
