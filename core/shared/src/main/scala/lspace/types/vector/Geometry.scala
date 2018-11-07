package lspace.types.vector

case class BBox(left: Double, bottom: Double, right: Double, top: Double) {
  def +(bbox: BBox): BBox =
    BBox(Vector(left, bbox.left).min,
         Vector(bottom, bbox.bottom).min,
         Vector(right, bbox.right).max,
         Vector(top, bbox.top).max)

  def contains(bbox: BBox): Boolean =
    left <= bbox.left && bottom <= bbox.bottom && right >= bbox.right && top >= bbox.top
  def within(bbox: BBox): Boolean = left >= bbox.left && bottom >= bbox.bottom && right <= bbox.right && top <= bbox.top
  def intersects(bbox: BBox): Boolean =
    (left - bbox.left) > (right - left) && (bottom - bbox.bottom) > (top - bottom) //test
}

object Geometry {
  implicit def toBBox(geometry: Geometry): BBox = geometry.bbox
}
trait Geometry extends Product with Serializable {
  def intersect(that: Geometry): Boolean
  def disjoint(that: Geometry): Boolean
  def contains(that: Geometry): Boolean
  def within(that: Geometry): Boolean

  def bbox: BBox
}

object Point {
  implicit def toPoint(xy: (Double, Double)): Point   = Point(xy._1, xy._2)
  implicit def toVector(point: Point): Vector[Double] = Vector(point.x, point.y)
}
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

object MultiPoint {
  def apply(points: Point*): MultiPoint = MultiPoint(points.toVector)
  implicit def toVector(point: MultiPoint): Vector[Vector[Double]] =
    point.vector.map(Point.toVector)
}
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

object Line {
  def apply(points: Point*): Line                             = Line(points.toVector)
  implicit def toVector(points: Line): Vector[Vector[Double]] = points.vector.map(Point.toVector)
}
case class Line(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.contains(that)
    case that: MultiPoint    => that.vector.exists(p => vector.contains(p))
    case that: Line          => bbox.intersects(that)
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

object MultiLine {
  def apply(points: Line*): MultiLine = MultiLine(points.toVector)
  implicit def toVector(points: MultiLine): Vector[Vector[Vector[Double]]] =
    points.vector.map(Line.toVector)
}
case class MultiLine(vector: Vector[Line]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.exists(_.vector.contains(that))
    case that: MultiPoint    => vector.exists(p => that.vector.contains(p))
    case that: Line          => vector.exists(_.bbox.intersects(that))
    case that: MultiLine     => that.vector.exists(line => vector.exists(_.bbox.intersects(line)))
    case that: Polygon       => vector.exists(line => that.bbox.intersects(line))
    case that: MultiPolygon  => vector.exists(line => that.vector.exists(_.bbox.intersects(line)))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !vector.exists(that.contains)
  def contains(that: Geometry): Boolean = that match {
    case that: Point      => vector.exists(_.bbox.contains(that))
    case that: MultiPoint => that.vector.forall(point => vector.exists(_.bbox.contains(point)))
    case that: Line       => vector.exists(_.vector.containsSlice(that.vector))
    case that: MultiLine  => that.vector.forall(line => vector.exists(_.bbox.intersects(line)))
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

object Polygon {
  def apply[T](points: T*)(implicit ev: T =:= Point): Polygon = Polygon(points.asInstanceOf[Seq[Point]].toVector)
  def apply[T: Numeric](points: (T, T)*)(implicit n: Numeric[T]): Polygon =
    Polygon(points.map(t => n.toDouble(t._1) -> n.toDouble(t._2)).map(Point.toPoint).toVector)
  implicit def toVector(points: Line): Vector[Vector[Double]] = points.vector.map(Point.toVector)
}
case class Polygon(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => bbox.intersects(that)
    case that: MultiPoint    => bbox.intersects(that)
    case that: Line          => bbox.intersects(that)
    case that: MultiLine     => bbox.intersects(that)
    case that: Polygon       => bbox.intersects(that)
    case that: MultiPolygon  => that.vector.exists(_.bbox.intersects(that))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !bbox.intersects(that)
  def contains(that: Geometry): Boolean = bbox.contains(that)
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

object MultiPolygon {
  def apply(points: Polygon*): MultiPolygon = MultiPolygon(points.toVector)
  implicit def toVector(points: MultiLine): Vector[Vector[Vector[Double]]] =
    points.vector.map(Polygon.toVector)
}
case class MultiPolygon(vector: Vector[Polygon]) extends Geometry {
  def intersect(that: Geometry): Boolean = vector.exists(geo => geo.intersect(that))
  def disjoint(that: Geometry): Boolean  = vector.forall(p => p.disjoint(that))
  def contains(that: Geometry): Boolean  = vector.forall(p => p.contains(that))
  def within(that: Geometry): Boolean    = vector.forall(p => p.within(that))

  lazy val bbox: BBox = BBox(
    vector.flatMap(_.vector.map(_.x)).min,
    vector.flatMap(_.vector.map(_.y)).min,
    vector.flatMap(_.vector.map(_.x)).max,
    vector.flatMap(_.vector.map(_.y)).max
  )
}

object MultiGeometry {
  def apply(points: Geometry*): MultiGeometry = MultiGeometry(points.toVector)
}
case class MultiGeometry(vector: Vector[Geometry]) extends Geometry {
  def intersect(that: Geometry): Boolean = vector.exists(geo => geo.intersect(that))
  def disjoint(that: Geometry): Boolean  = vector.forall(geo => geo.disjoint(that))
  def contains(that: Geometry): Boolean  = vector.forall(geo => geo.contains(that))
  def within(that: Geometry): Boolean    = vector.forall(geo => geo.within(that))

  lazy val bbox: BBox = vector.map(_.bbox).reduce(_ + _)
}
