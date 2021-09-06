package lspace.provider.transaction

import lspace.provider.mem.MemValue

trait TValue[T] extends MemValue[T] with TResource[T]
