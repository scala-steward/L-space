package lspace.librarian.provider.transaction

import lspace.librarian.provider.mem.MemValue
import lspace.librarian.structure.{Graph, Value}

trait TValue[T] extends MemValue[T] with TResource[T]
