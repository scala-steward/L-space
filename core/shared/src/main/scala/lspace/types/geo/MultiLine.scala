package lspace.types.vector

case class MultiLine(vector: Vector[Line]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.exists(_.vector.contains(that))
    case that: MultiPoint    => vector.exists(p => that.vector.contains(p))
    case that: Line          => vector.exists(_.bbox.intersects(that.bbox))
    case that: MultiLine     => that.vector.exists(line => vector.exists(_.bbox.intersects(line.bbox)))
    case that: Polygon       => vector.exists(line => that.bbox.intersects(line.bbox))
    case that: MultiPolygon  => vector.exists(line => that.vector.exists(_.bbox.intersects(line.bbox)))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !vector.exists(that.contains)
  def contains(that: Geometry): Boolean = that match {
    case that: Point      => vector.exists(_.bbox.contains(that.bbox))
    case that: MultiPoint => that.vector.forall(point => vector.exists(_.bbox.contains(point.bbox)))
    case that: Line       => vector.exists(_.vector.containsSlice(that.vector))
    case that: MultiLine  => that.vector.forall(line => vector.exists(_.bbox.intersects(line.bbox)))
    case _                => false
  }
  def within(that: Geometry): Boolean = that match {
    case that: Point         => false
    case that: MultiPoint    => false
    case that: Line          => vector.forall(p => that.containsSlice(p.vector))
    case that: MultiLine     => vector.forall(p => that.vector.exists(_.vector.containsSlice(p.vector)))
    case that: Polygon       => vector.forall(p => that.contains(p))
    case that: MultiPolygon  => vector.forall(p => that.vector.exists(_.contains(p)))
    case that: MultiGeometry => that.contains(this)
  }

  lazy val bbox: BBox = BBox(
    vector.flatMap(_.vector.map(_.x)).min,
    vector.flatMap(_.vector.map(_.y)).min,
    vector.flatMap(_.vector.map(_.x)).max,
    vector.flatMap(_.vector.map(_.y)).max
  )
}

object MultiLine {
  def apply(points: Line*): MultiLine = MultiLine(points.toVector)
  implicit def toVector(points: MultiLine): Vector[Vector[Vector[Double]]] =
    points.vector.map(Line.toVector)
}
