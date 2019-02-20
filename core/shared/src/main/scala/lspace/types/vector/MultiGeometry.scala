package lspace.types.vector

case class MultiGeometry(vector: Vector[Geometry]) extends Geometry {
  def intersect(that: Geometry): Boolean = vector.exists(geo => geo.intersect(that))
  def disjoint(that: Geometry): Boolean  = vector.forall(geo => geo.disjoint(that))
  def contains(that: Geometry): Boolean  = vector.forall(geo => geo.contains(that))
  def within(that: Geometry): Boolean    = vector.forall(geo => geo.within(that))

  lazy val bbox: BBox = vector.map(_.bbox).reduce(_ + _)
}

object MultiGeometry {
  def apply(points: Geometry*): MultiGeometry = MultiGeometry(points.toVector)
}
