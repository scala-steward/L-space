package lspace.codec

import lspace._
import Label.D
import Label.P

object ActiveProperty {
  lazy val `@id`    = ActiveProperty(P.`@id`, D.`@string` :: Nil)()
  lazy val `@ids`   = ActiveProperty(P.`@ids`, D.`@string` :: Nil, `@container` = `@container`.`@set` :: Nil)()
  lazy val `@label` = ActiveProperty(P.`@label`, D.`@string` :: Nil)()
  lazy val `@labelLang` =
    ActiveProperty(P.`@label`, D.`@string` :: Nil, `@container` = `@container`.`@language` :: Nil)()
  lazy val `@comment` = ActiveProperty(P.`@comment`, D.`@string` :: Nil)()
  lazy val `@commentLang` =
    ActiveProperty(P.`@comment`, D.`@string` :: Nil, `@container` = `@container`.`@language` :: Nil)()
  lazy val `@type` = ActiveProperty(P.`@type`)()

}
case class ActiveProperty(property: Property,
                          `@type`: List[ClassType[_]] = Nil,
                          `@container`: List[`@container`] = Nil,
                          `@reverse`: Boolean = false)(val `@context`: ActiveContext = ActiveContext()) {

  def copy(property: Property = this.property,
           `@type`: List[ClassType[_]] = this.`@type`,
           `@container`: List[`@container`] = this.`@container`,
           `@reverse`: Boolean = this.`@reverse`)(`@context`: ActiveContext = this.`@context`): ActiveProperty = {
    ActiveProperty(property, `@type`, `@container`, `@reverse`)(`@context`)
  }
  //TODO: throw exception when property.iri != activeProperty.property.iri ???
  def ++(activeProperty: ActiveProperty): ActiveProperty =
    this.copy(
      `@type` = `@type` ++ activeProperty.`@type`,
      `@container` = `@container` ++ activeProperty.`@container`,
      `@reverse` = activeProperty.`@reverse` || `@reverse`
    )(`@context` ++ activeProperty.`@context`)
}
