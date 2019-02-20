package lspace.types.vector

object Geometry {
//  implicit def toBBox(geometry: Geometry): BBox = geometry.bbox
}
trait Geometry extends Product with Serializable {
  def intersect(that: Geometry): Boolean
  def disjoint(that: Geometry): Boolean
  def contains(that: Geometry): Boolean
  def within(that: Geometry): Boolean

  def bbox: BBox
}
