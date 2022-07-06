package lspace.structure.util

import monix.eval.Task

/** id's should start from 1000, the first 1000 longs are reserved for default-classtypes
  */
trait IdProvider {
  def next: Task[Long]
}
