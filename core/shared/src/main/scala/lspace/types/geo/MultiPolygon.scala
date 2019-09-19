package lspace.types.vector

case class MultiPolygon(vector: Vector[Polygon]) extends Geometry {
  def intersect(that: Geometry): Boolean = vector.exists(geo => geo.intersect(that))
  def disjoint(that: Geometry): Boolean  = vector.forall(p => p.disjoint(that))
  def contains(that: Geometry): Boolean  = vector.forall(p => p.contains(that))
  def within(that: Geometry): Boolean    = vector.forall(p => p.within(that))

  lazy val bbox: BBox = BBox(
    vector.flatMap(_.vector.flatMap(_.map(_.x))).min,
    vector.flatMap(_.vector.flatMap(_.map(_.y))).min,
    vector.flatMap(_.vector.flatMap(_.map(_.x))).max,
    vector.flatMap(_.vector.flatMap(_.map(_.y))).max
  )
}

object MultiPolygon {
  def apply(points: Polygon*): MultiPolygon = MultiPolygon(points.toVector)
  implicit def toVector(points: MultiLine): Vector[Vector[Vector[Double]]] =
    points.vector.map(Polygon.toVector)
}
