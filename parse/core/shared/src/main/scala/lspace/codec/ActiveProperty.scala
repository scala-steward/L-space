package lspace.codec

import lspace.structure.{ClassType, Property}

case class ActiveProperty(`@context`: ActiveContext = ActiveContext(),
                          `@type`: List[ClassType[_]] = Nil,
                          `@container`: List[`@container`] = Nil,
                          `@reverse`: Boolean = false,
                          property: Property) {

  def ++(activeProperty: ActiveProperty): ActiveProperty =
    this.copy(
      `@context` = `@context` ++ activeProperty.`@context`,
      `@type` = `@type` ++ activeProperty.`@type`,
      `@container` = `@container` ++ activeProperty.`@container`,
      `@reverse` = activeProperty.`@reverse` || `@reverse`
    )
}
