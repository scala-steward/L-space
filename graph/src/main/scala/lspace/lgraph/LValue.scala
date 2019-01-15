package lspace.lgraph

import lspace.librarian.structure.Value

object LValue {}

trait LValue[T] extends LResource[T] with Value[T]
