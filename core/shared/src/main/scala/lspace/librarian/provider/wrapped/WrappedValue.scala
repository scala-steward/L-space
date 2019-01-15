package lspace.librarian.provider.wrapped

import lspace.librarian.datatype.DataType
import lspace.librarian.structure._
import lspace.util.CacheStatus

abstract class WrappedValue[T](override val self: Value[T]) extends Value[T] with WrappedResource[T] {

  override val value: T           = self.value
  override val label: DataType[T] = self.label
}
