package lspace.types.vector

case class MultiPoint(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.contains(that)
    case that: MultiPoint    => vector.exists(p => that.vector.contains(p))
    case that: Line          => that.contains(this)
    case that: MultiLine     => that.vector.exists(_.contains(this))
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !vector.exists(that.contains)
  def contains(that: Geometry): Boolean = vector.contains(that)
  def within(that: Geometry): Boolean = that match {
    case that: Point         => vector.forall(_ == that)
    case that: MultiPoint    => vector.forall(p => that.vector.contains(p))
    case that: Line          => vector.forall(p => that.vector.contains(p))
    case that: MultiLine     => vector.forall(p => that.vector.exists(_.vector.contains(p)))
    case that: Polygon       => vector.forall(p => that.contains(p))
    case that: MultiPolygon  => vector.forall(p => that.vector.exists(_.contains(p)))
    case that: MultiGeometry => that.contains(this)
  }

  lazy val bbox: BBox = BBox(vector.map(_.x).min, vector.map(_.y).min, vector.map(_.x).max, vector.map(_.y).max)
}

object MultiPoint {
  def apply(points: Point*): MultiPoint = MultiPoint(points.toVector)
  implicit def toVector(point: MultiPoint): Vector[Vector[Double]] =
    point.vector.map(Point.toVector)
}
