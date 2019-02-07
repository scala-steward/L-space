package lspace.encode

import monix.eval.Task

trait Encode[A] {
  def encode: A => String
}
