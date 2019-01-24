package lspace.parse

import lspace.librarian.structure.{ClassType, Property}

object ActiveProperty {
  def apply(`@type`: ClassType[_]): ActiveProperty =
    new ActiveProperty(ActiveContext(), `@type` :: Nil, Nil)
  def apply(`@container`: `@container`): ActiveProperty =
    new ActiveProperty(ActiveContext(), Nil, `@container` :: Nil)
}
case class ActiveProperty(`@context`: ActiveContext = ActiveContext(),
                          `@type`: List[ClassType[_]] = Nil,
                          `@container`: List[`@container`] = Nil) {
  def `@prefix`: Map[String, String]            = `@context`.`@prefix`
  def `@vocab`: Option[String]                  = `@context`.`@vocab`
  def properties: Map[Property, ActiveProperty] = `@context`.properties
}
