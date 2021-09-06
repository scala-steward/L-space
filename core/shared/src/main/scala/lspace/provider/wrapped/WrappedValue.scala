package lspace.provider.wrapped

import lspace.structure._

abstract class WrappedValue[T](override val self: Value[T]) extends Value[T] with WrappedResource[T] {

  override val value: T = self.value
  def label             = self.label
}
