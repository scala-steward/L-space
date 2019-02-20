package lspace.types.vector

case class Line(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.contains(that)
    case that: MultiPoint    => that.vector.exists(p => vector.contains(p))
    case that: Line          => bbox.intersects(that.bbox)
    case that: MultiLine     => that.vector.exists(_.bbox.intersects(bbox))
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !that.contains(this)
  def contains(that: Geometry): Boolean = that match {
    case that: Point      => vector.contains(that)
    case that: MultiPoint => that.vector.forall(vector.contains)
    case that: Line       => vector.containsSlice(that.vector)
    case that: MultiLine  => that.vector.map(_.vector).forall(vector.containsSlice)
    case _                => false
  }
  def within(that: Geometry): Boolean = that match {
    case that: Point         => false
    case that: MultiPoint    => that.vector.contains(this)
    case that: Line          => that.vector.contains(this)
    case that: MultiLine     => that.vector.exists(_.vector.contains(this))
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.contains(this)
  }

  lazy val bbox: BBox = BBox(vector.map(_.x).min, vector.map(_.y).min, vector.map(_.x).max, vector.map(_.y).max)
}

object Line {
  def apply(points: Point*): Line                             = Line(points.toVector)
  implicit def toVector(points: Line): Vector[Vector[Double]] = points.vector.map(Point.toVector)
}
