package lspace.lgraph

import lspace.structure.Value

object LValue {}

trait LValue[T] extends LResource[T] with Value[T]
