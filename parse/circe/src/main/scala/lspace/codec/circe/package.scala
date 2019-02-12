package lspace.codec

package object circe {
  implicit val nativeEncoder: lspace.codec.NativeTypeEncoder.Aux[_root_.io.circe.Json] = circe.NativeTypeEncoder
  implicit val nativeDecoder: lspace.codec.NativeTypeDecoder.Aux[_root_.io.circe.Json] = circe.NativeTypeDecoder
}
