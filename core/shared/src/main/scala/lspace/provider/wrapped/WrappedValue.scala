package lspace.provider.wrapped

import lspace.datatype.DataType
import lspace.structure._
import lspace.util.CacheStatus

abstract class WrappedValue[T](override val self: Value[T]) extends Value[T] with WrappedResource[T] {

  override val value: T = self.value
  def label             = self.label
}
