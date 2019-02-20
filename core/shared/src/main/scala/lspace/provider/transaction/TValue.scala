package lspace.provider.transaction

import lspace.provider.mem.MemValue
import lspace.structure.{Graph, Value}

trait TValue[T] extends MemValue[T] with TResource[T]
