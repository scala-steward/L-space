package lspace.codec

package object argonaut {
  implicit val nativeEncoder: lspace.codec.NativeTypeEncoder.Aux[_root_.argonaut.Json] = argonaut.NativeTypeEncoder
  implicit val nativeDecoder: lspace.codec.NativeTypeDecoder.Aux[_root_.argonaut.Json] = argonaut.NativeTypeDecoder
}
