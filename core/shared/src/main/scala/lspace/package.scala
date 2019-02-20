import lspace.librarian.logic.{Assistent, DefaultAssistent}
import lspace.librarian.task
import lspace.librarian.task.{Guide, StandardGuide}
import lspace.structure.ClassType
import lspace.structure.util.ClassTypeable
import shapeless.HList

package object lspace {

  type P[Z] = lspace.librarian.logic.predicate.P[Z]
  val P = lspace.librarian.logic.predicate.P

  type Traversal[+ST <: ClassType[_], +ET <: ClassType[_], Steps <: HList] =
    lspace.librarian.traversal.Traversal[ST, ET, Steps]
  implicit def tToT[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList](
      t: Traversal[ST, ET, Steps]): lspace.librarian.traversal.Traversal[ST, ET, Steps] = t

//  type Traversal[Z] = lspace.librarian.traversal.Traversal[Z]
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
    object StandardGuide {
      implicit val guide: Guide = task.StandardGuide()(DefaultAssistent.assistent)
    }
  }
}
