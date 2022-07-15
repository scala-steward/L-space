package lspace.codec

trait Encoder[H, L]:
  def encode(value: H): L
