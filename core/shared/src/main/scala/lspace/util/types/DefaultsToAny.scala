package lspace.util.types

sealed class DefaultsToAny[A]

object DefaultsToAny {
  implicit def overrideDefault[A]: DefaultsToAny[A] = new DefaultsToAny[A]

  implicit def default: DefaultsToAny[Any] = new DefaultsToAny[Any]
}
