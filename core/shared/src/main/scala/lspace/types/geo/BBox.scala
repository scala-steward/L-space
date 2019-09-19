package lspace.types.vector

case class BBox(left: Double, bottom: Double, right: Double, top: Double) {
  lazy val width  = Math.abs(left - right)
  lazy val height = Math.abs(bottom - top)
  def +(bbox: BBox): BBox =
    BBox(Vector(left, bbox.left).min,
         Vector(bottom, bbox.bottom).min,
         Vector(right, bbox.right).max,
         Vector(top, bbox.top).max)

  def contains(bbox: BBox): Boolean =
    left <= bbox.left && bottom <= bbox.bottom && right >= bbox.right && top >= bbox.top
  def within(bbox: BBox): Boolean = left >= bbox.left && bottom >= bbox.bottom && right <= bbox.right && top <= bbox.top
  def intersects(bbox: BBox): Boolean = {
    (Math.abs(bbox.left - left) * 2 < (bbox.width + width)) &&
    (Math.abs(bbox.bottom - bottom) * 2 < (bbox.height + height))
  }
}
