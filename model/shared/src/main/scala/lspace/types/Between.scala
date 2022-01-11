package lspace.types

opaque type Between[min, max] <: NonLit[min] = NonLit[min]

object Between extends Interval:
  type C[min, max] = Between[min, max]

  import scala.compiletime.ops.boolean.*
  import scala.compiletime.ops.int
  import scala.compiletime.ops.double
  import scala.compiletime.ops.long

  type Validate[n, min, max] <: Boolean = (n, min, max) match {
    case (Int, Int, Int)          => (int.>=[n, min] && int.<=[n, max])
    case (Double, Double, Double) => (double.>=[n, min] && double.<=[n, max])
    case (Long, Long, Long)       => (long.>=[n, min] && long.<=[n, max])
  }
  def Validate[n, min, max](n: n, min: min, max: max): Either[String, n] =
    if (
      (n, min, max) match {
        case (n: Int, min: Int, max: Int)          => n >= min && n <= max
        case (n: Double, min: Double, max: Double) => n >= min && n <= max
        case (n: Long, min: Long, max: Long)       => n >= min && n <= max
        case _                                     => false
      }
    ) Right(n)
    else
      Left(s"$n is not between $min and $max")