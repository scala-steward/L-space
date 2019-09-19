package lspace.librarian.traversal.util

import lspace.librarian.traversal.step.As
import shapeless.Poly1

object LabelStepTypes extends Poly1 {
  implicit def getType[T, name <: String] = at[As[T, name]](t => t._maphelper)
}
