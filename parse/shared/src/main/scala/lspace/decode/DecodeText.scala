package lspace.decode

import monix.eval.Task

trait DecodeText[A] extends Decode[A] {
  def decode(json: String): Task[A]
}

object DecodeText {}
