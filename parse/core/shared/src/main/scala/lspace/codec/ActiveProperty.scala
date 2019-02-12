package lspace.codec

import lspace.librarian.structure.{ClassType, Property}

//trait ActiveProperty[Json] {
//  def `@context`: ActiveContext[Json]
//  def `@type`: List[ClassType[_]]
//  def `@container`: List[`@container`]
//  def `@prefix`: Map[String, String]                  = `@context`.`@prefix`
//  def `@vocab`: Option[String]                        = `@context`.`@vocab`
//  def properties: Map[Property, ActiveProperty[Json]] = `@context`.properties
//
//  def copy(`@context`: ActiveContext[Json] = `@context`,
//           `@type`: List[ClassType[_]] = `@type`,
//           `@container`: List[`@container`] = `@container`): ActiveProperty[Json]
//}

case class ActiveProperty(`@context`: ActiveContext = ActiveContext(),
                          `@type`: List[ClassType[_]] = Nil,
                          `@container`: List[`@container`] = Nil) {
//  override def copy(`@context`: ActiveContext[Json],
//                    `@type`: List[ClassType[_]],
//                    `@container`: List[`@container`]): ActiveProperty[Json] =
//    ActiveProperty(`@context`, `@type`, `@container`)
}
