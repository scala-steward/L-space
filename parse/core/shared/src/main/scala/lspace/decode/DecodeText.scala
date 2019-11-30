package lspace.decode

trait DecodeText[A, F[_]] extends Decode[A, F] {
  type In = String
}

object DecodeText {
  type Aux[Out, F[_], In0] = DecodeText[Out, F] { type In = In0 }
}
