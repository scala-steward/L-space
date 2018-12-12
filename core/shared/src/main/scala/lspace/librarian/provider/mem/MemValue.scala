package lspace.librarian.provider.mem

import lspace.librarian.structure.{DataType, Value}

object MemValue {}

trait MemValue[T] extends MemResource[T] with Value[T]
