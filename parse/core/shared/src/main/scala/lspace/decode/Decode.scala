package lspace.decode

import monix.eval.Task

trait Decode[A] {
  def decode: String => Task[A]
}
