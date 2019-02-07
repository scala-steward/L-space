package lspace.codec

import lspace.librarian.structure.{ClassType, Property}

trait ActiveProperty[Json, JsonObject] {
  def `@context`: ActiveContext[Json, JsonObject]
  def `@type`: List[ClassType[_]]
  def `@container`: List[`@container`]
  def `@prefix`: Map[String, String]                              = `@context`.`@prefix`
  def `@vocab`: Option[String]                                    = `@context`.`@vocab`
  def properties: Map[Property, ActiveProperty[Json, JsonObject]] = `@context`.properties

  def copy(`@context`: ActiveContext[Json, JsonObject] = `@context`,
           `@type`: List[ClassType[_]] = `@type`,
           `@container`: List[`@container`] = `@container`): ActiveProperty[Json, JsonObject]
}
