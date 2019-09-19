package lspace.types.vector

case class Point(x: Double, y: Double) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => that == this
    case that: MultiPoint    => that.vector.contains(this)
    case that: Line          => that.contains(this)
    case that: MultiLine     => that.vector.exists(_.contains(this))
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !that.contains(this)
  def contains(that: Geometry): Boolean = this == that
  def within(that: Geometry): Boolean = that match {
    case that: Point         => that == this
    case that: MultiPoint    => that.vector.contains(this)
    case that: Line          => that.contains(this)
    case that: MultiLine     => that.vector.exists(_.contains(this))
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.contains(this)
  }

  lazy val bbox: BBox = BBox(x, y, x, y)
}

object Point {
  implicit def toPoint(xy: (Double, Double)): Point   = Point(xy._1, xy._2)
  implicit def toVector(point: Point): Vector[Double] = Vector(point.x, point.y)
}
