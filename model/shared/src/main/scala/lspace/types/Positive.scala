package lspace.types

opaque type Positive = Int

object Positive:
  import scala.compiletime.ops.any.IsConst
  import scala.compiletime.ops.int.*

  type Id[X]        = X
  type N            = Int with Singleton
  type Able[n <: N] = (n > 0) =:= true

  private def unsafe[n <: Int](n: n): Positive = n

  def apply[n <: Int](n: n)(using IsConst[n] =:= false): Either[String, Positive] =
    if (n > 0) Right(unsafe(n))
    else Left(s"$n is not a positive value, a positive value a natural number, which is an integer greater than 0")

  given intToPositive[n <: N](using Able[n]): Conversion[n, Positive] =
    unsafe[n](_)

  extension (n: Positive) inline def unapply: Int = n
