package lspace.types

type NonLit[n] = n match
  case Int    => Int
  case Double => Double
  case Long   => Long
  case String => String

type Positive[n] = n match {
  case Int    => Gt[0]
  case Double => Gt[0.0]
  case Long   => Gt[0L]
}
object Positive:
  type Able[x] = Positive[x] match
    case Gt[t] => Gt.Able[x, t]

type NonPositive[n] = n match {
  case Int    => Lte[0]
  case Double => Lte[0.0]
  case Long   => Lte[0L]
}
object NonPositive:
  type Able[x] = NonPositive[x] match
    case Lte[t] => Lte.Able[x, t]

type Negative[n] = n match {
  case Int    => Lt[0]
  case Double => Lt[0.0]
  case Long   => Lt[0L]
}
object Negative:
  type Able[x] = Negative[x] match
    case Lt[t] => Lt.Able[x, t]

type NonNegative[n] = n match {
  case Int    => Gte[0]
  case Double => Gte[0.0]
  case Long   => Gte[0L]
}
object NonNegative:
  type Able[x] = NonNegative[x] match
    case Gte[t] => Gte.Able[x, t]

type Probability = Between[Probability.Min, Probability.Max]

object Probability:

  type P   = Double with Singleton
  type Min = 0.0
  type Max = 1.0

  def safeApply(p: Double): Either[String, Probability] =
    Between.safeApply(p, implicitly[ValueOf[Min]].value, implicitly[ValueOf[Max]].value)

  def apply[p <: P](p: p)(using Conversion[p, Between[Min, Max]]): Probability = p
