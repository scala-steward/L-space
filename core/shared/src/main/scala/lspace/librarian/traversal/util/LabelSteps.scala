package lspace.librarian.traversal.util

import lspace.librarian.traversal.step.As
import shapeless.Poly1

object LabelSteps extends Poly1 {
  implicit def as[T, name <: String]: Case.Aux[As[T, name], As[T, name]] = at[As[T, name]](s => s)
}
