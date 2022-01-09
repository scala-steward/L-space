package lspace.types

trait Interval:
  type C[min, max] <: NonLit[min]

  type Able[n, min, max] = Validate[n, min, max] =:= true
  type Validate[n, min, max] <: Boolean
  def Validate[n, min, max](n: n, min: min, max: max): Either[String, n]

  private def unsafe[n, min, max](n: n): C[min, max] = n.asInstanceOf[C[min, max]]

  def apply[n <: Singleton, min <: Singleton, max <: Singleton](n: n)(using
    Conversion[n, C[min, max]]
  ): C[min, max] = n

  def safeApply[n, min, max](n: n)(using
    scala.compiletime.ops.any.IsConst[n] =:= false,
    ValueOf[min],
    ValueOf[max]
  ): Either[String, C[min, max]] =
    Validate(n, implicitly[ValueOf[min]].value, implicitly[ValueOf[max]].value).map(unsafe)

  given toInterval[n <: Singleton, min <: Singleton, max <: Singleton](using
    Able[n, min, max]
  ): Conversion[n, C[min, max]] =
    unsafe[n, min, max](_)

  extension [n, min, max](n: C[min, max]) inline def unapply: NonLit[min] = n
