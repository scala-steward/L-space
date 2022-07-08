package lspace.types

import scala.annotation.experimental

trait Compare:
  type C[t] <: NonLit[t]
  type Able[x, y] = Validate[x, y] =:= true
  type Validate[x, y] <: Boolean
  def Validate[x, y](x: x, y: y): Either[String, x]

  private def unsafe[x, y](x: x): C[y] = x.asInstanceOf[C[y]]

  def apply[x <: Singleton, y <: Singleton](x: x)(using
    Conversion[x, C[y]]
  ): C[y] = x

  def safeApply[x, y](x: x)(using scala.compiletime.ops.any.IsConst[x] =:= false, ValueOf[y]): Either[String, C[y]] =
    Validate(x, implicitly[ValueOf[y]].value).map(unsafe)

  given toCompare[x <: Singleton, y <: Singleton](using Able[x, y]): Conversion[x, C[y]] =
    unsafe[x, y](_)

  extension [n](n: C[n]) inline def unapply: NonLit[n] = n
