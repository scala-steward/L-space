package lspace.datatype.util

object Ordering {
  val Double: scala.math.Ordering[Double] = scala.math.Ordering.Double.IeeeOrdering
  val Float: scala.math.Ordering[Float]   = scala.math.Ordering.Float.IeeeOrdering
}
