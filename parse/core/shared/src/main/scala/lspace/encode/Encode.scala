package lspace.encode

trait Encode[A] {
  def encode: A => String
}
