package lspace.types

opaque type Lte[n] <: NonLit[n] = NonLit[n]

object Lte extends Compare:
  type C[t] = Lte[t]

  type Validate[x, y] <: Boolean = (x, y) match {
    case (Int, Int)       => scala.compiletime.ops.int.<=[x, y]
    case (Double, Double) => scala.compiletime.ops.double.<=[x, y]
    case (Long, Long)     => scala.compiletime.ops.long.<=[x, y]
  }
  def Validate[x, y](x: x, y: y): Either[String, x] =
    if (
      (x, y) match {
        case (x: Int, y: Int)       => x <= y
        case (x: Double, y: Double) => x <= y
        case (x: Long, y: Long)     => x <= y
        case _                      => false
      }
    ) Right(x)
    else
      Left(s"$x is not a value less or equal than $y")
