package lspace.encode

import lspace.codec.ActiveContext

trait Encode[In, F[_]] {
  type Out
  def encode(implicit activeContext: ActiveContext): In => F[Out]
}

object Encode {
  type Aux[In, F[_], Out0] = Encode[In, F] { type Out = Out0 }
}
