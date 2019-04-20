package lspace.encode

import lspace.codec.ActiveContext

trait Encode[A] {
  def encode(implicit activeContext: ActiveContext): A => String
}
