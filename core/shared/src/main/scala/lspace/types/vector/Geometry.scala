package lspace.types.vector

trait Geometry extends Product with Serializable {
  def intersect(that: Geometry): Boolean
  def disjoint(that: Geometry): Boolean
  def contains(that: Geometry): Boolean
  def within(that: Geometry): Boolean
}

object Point {
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
}

object Line {
  def apply(points: Point*): Line                             = Line(points.toVector)
  implicit def toVector(points: Line): Vector[Vector[Double]] = points.vector.map(Point.toVector)
}
case class Line(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.contains(that)
    case that: MultiPoint    => that.vector.exists(p => vector.contains(p))
    case that: Line          => that.vector.contains(this)
    case that: MultiLine     => that.vector.exists(_.vector.contains(this))
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !that.contains(this)
  def contains(that: Geometry): Boolean = that match {
    case that: Point      => ???
    case that: MultiPoint => ???
    case that: Line       => ???
    case that: MultiLine  => ???
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
}

object MultiLine {
  def apply(points: Line*): MultiLine = MultiLine(points.toVector)
  implicit def toVector(points: MultiLine): Vector[Vector[Vector[Double]]] =
    points.vector.map(Line.toVector)
}
case class MultiLine(vector: Vector[Line]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => vector.contains(that)
    case that: MultiPoint    => vector.exists(p => that.vector.contains(p))
    case that: Line          => ???
    case that: MultiLine     => ???
    case that: Polygon       => ???
    case that: MultiPolygon  => ???
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = !vector.exists(that.contains)
  def contains(that: Geometry): Boolean = that match {
    case that: Point      => ???
    case that: MultiPoint => ???
    case that: Line       => ???
    case that: MultiLine  => ???
    case _                => false
  }
  def within(that: Geometry): Boolean = that match {
    case that: Point         => false
    case that: MultiPoint    => false
    case that: Line          => vector.forall(p => that.contains(p))
    case that: MultiLine     => vector.forall(p => that.vector.exists(_.vector.contains(p)))
    case that: Polygon       => vector.forall(p => that.contains(p))
    case that: MultiPolygon  => vector.forall(p => that.vector.exists(_.contains(p)))
    case that: MultiGeometry => that.contains(this)
  }
}

object Polygon {
  def apply(points: Point*): Polygon                          = Polygon(points.toVector)
  implicit def toVector(points: Line): Vector[Vector[Double]] = points.vector.map(Point.toVector)
}
case class Polygon(vector: Vector[Point]) extends Geometry {
  def intersect(that: Geometry): Boolean = that match {
    case that: Point         => ???
    case that: MultiPoint    => ???
    case that: Line          => ???
    case that: MultiLine     => ???
    case that: Polygon       => ???
    case that: MultiPolygon  => ???
    case that: MultiGeometry => that.intersect(this)
  }
  def disjoint(that: Geometry): Boolean = ???
  def contains(that: Geometry): Boolean = ???
  def within(that: Geometry): Boolean = that match {
    case that: Point         => false
    case that: MultiPoint    => false
    case that: Line          => false
    case that: MultiLine     => false
    case that: Polygon       => that.contains(this)
    case that: MultiPolygon  => that.vector.exists(_.contains(this))
    case that: MultiGeometry => that.contains(this)
  }
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
}

object MultiGeometry {
  def apply(points: Geometry*): MultiGeometry = MultiGeometry(points.toVector)
}
case class MultiGeometry(vector: Vector[Geometry]) extends Geometry {
  def intersect(that: Geometry): Boolean = vector.exists(geo => geo.intersect(that))
  def disjoint(that: Geometry): Boolean  = vector.forall(geo => geo.disjoint(that))
  def contains(that: Geometry): Boolean  = vector.forall(geo => geo.contains(that))
  def within(that: Geometry): Boolean    = vector.forall(geo => geo.within(that))
}
