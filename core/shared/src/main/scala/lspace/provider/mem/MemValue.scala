package lspace.provider.mem

import lspace.structure.Value

object MemValue {}

trait MemValue[T] extends MemResource[T] with Value[T]
