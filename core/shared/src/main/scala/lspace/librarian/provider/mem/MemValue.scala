package lspace.librarian.provider.mem

import lspace.librarian.structure.Value

object MemValue {}

trait MemValue[T] extends MemResource[T] with Value[T]
