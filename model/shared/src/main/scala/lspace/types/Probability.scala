package lspace.types

opaque type Probability = Double

object Probability:
  import scala.compiletime.ops.any.IsConst
  import scala.compiletime.ops.boolean.*
  import scala.compiletime.ops.double.*

  type Id[X]                    = X
  type P                        = Double with Singleton
  type Min                      = 0.0
  type Max                      = 1.0
  type Able[p <: P] = ((p >= Min) && (p <= Max)) =:= true

  // def apply[p <: Double](p: p)(using Able[p]): Probability = p

  private def unsafe[p <: Double](p: p): Probability = p

  def apply[p <: Double](p: p)(using IsConst[p] =:= false): Either[String, Probability] =
    if (p >= 0.0 && p <= 1.0) Right(unsafe(p))
    else Left(s"$p is not a probability, a probability is a value between 0.0 and 1.0")

  given doubleToProbability[p <: P](using Able[p]): Conversion[p, Probability] =
    unsafe[p](_)

  extension (p: Probability) inline def unapply: Double = p
