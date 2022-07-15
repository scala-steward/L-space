package lspace.codec

trait Decoder[H, L]:
  def decode(value: L): Either[Throwable, H]
