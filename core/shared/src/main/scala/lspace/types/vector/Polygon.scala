package lspace.types.vector

case class Polygon(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => bbox.intersects(that.bbox)
    case that: MultiPoint    => bbox.intersects(that.bbox)
    case that: Line          => bbox.intersects(that.bbox)
    case that: MultiLine     => bbox.intersects(that.bbox)
    case that: Polygon       => bbox.intersects(that.bbox)
    case that: MultiPolygon  => that.vector.exists(_.bbox.intersects(that.bbox))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !bbox.intersects(that.bbox)
  def contains(that: Geometry): Boolean = bbox.contains(that.bbox)
  def within(that: Geometry): Boolean = that match {
    case that: Point         => false
    case that: MultiPoint    => false
    case that: Line          => false
    case that: MultiLine     => false
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.contains(this)
  }

  lazy val bbox: BBox = BBox(vector.map(_.x).min, vector.map(_.y).min, vector.map(_.x).max, vector.map(_.y).max)
}

object Polygon {
  def apply[T](points: T*)(implicit ev: T =:= Point): Polygon = Polygon(points.asInstanceOf[Seq[Point]].toVector)
  def apply[T: Numeric](points: (T, T)*)(implicit n: Numeric[T]): Polygon =
    Polygon(points.map(t => n.toDouble(t._1) -> n.toDouble(t._2)).map(Point.toPoint).toVector)
  implicit def toVector(points: Line): Vector[Vector[Double]] = points.vector.map(Point.toVector)
}
