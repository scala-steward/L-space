package lspace.datatype.util

import scala.math.Ordering
object Ordering {
  val Double: scala.math.Ordering[Double] = scala.math.Ordering.Double.IeeeOrdering
  val Float: scala.math.Ordering[Float] = scala.math.Ordering.Float.IeeeOrdering
}
