package lspace.lgraph

import lspace.librarian.structure.{DataType, Value}

object LValue {}

trait LValue[T] extends LResource[T] with Value[T]
