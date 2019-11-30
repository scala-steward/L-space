package lspace.decode

import lspace.codec.ActiveContext

trait Decode[Out, F[_]] {
  type In
  def decode(implicit activeContext: ActiveContext): In => F[Out]
}

object Decode {
  type Aux[Out, F[_], In0] = Decode[Out, F] { type In = In0 }
}
