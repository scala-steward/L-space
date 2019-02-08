package lspace.parse

//import argonaut.{Json, JsonObject}
//import lspace.codec
//import lspace.codec.`@container`
//import lspace.librarian.structure.{ClassType, Property}

//object ActiveProperty {
//  def apply(`@type`: ClassType[_]): ActiveProperty =
//    new ActiveProperty(ActiveContext(), `@type` :: Nil, Nil)
//  def apply(`@container`: `@container`): ActiveProperty =
//    new ActiveProperty(ActiveContext(), Nil, `@container` :: Nil)
//}
//case class ActiveProperty(`@context`: codec.ActiveContext[Json, JsonObject] = ActiveContext(),
//                          `@type`: List[ClassType[_]] = Nil,
//                          `@container`: List[`@container`] = Nil)
//    extends lspace.codec.ActiveProperty[Json, JsonObject] {
//  override def copy(`@context`: codec.ActiveContext[Json, JsonObject],
//                    `@type`: List[ClassType[_]],
//                    `@container`: List[`@container`]): codec.ActiveProperty[Json, JsonObject] =
//    ActiveProperty(`@context`, `@type`, `@container`)
//}
