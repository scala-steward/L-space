package lspace.librarian.structure

object PropertyDef {
  implicit def pDefToProperty(df: PropertyDef): Property = df.property
}

/**
  *
  * @param iri
  * @param label
  * @param comment
  * @param iris
  * @param container
  * @param `@range`
  * @param `@extends`
  */
abstract class PropertyDef(iri: String,
                           label: String,
                           comment: String = "",
                           iris: Set[String] = Set(),
                           container: List[String] = List(),
                           `@range`: () => List[ClassType[_]] = () => List(),
                           `@extends`: () => List[Property] = () => List())
    extends ClassTypeDef[Property] {

  def classtype = property

  lazy val property: Property =
    new Property(
      iri,
      iris,
      _range = `@range`,
      containers = container,
      _properties = () => properties,
      label = Map("en" -> label),
      comment = Map("en" -> comment).filter(_._2.nonEmpty),
      _extendedClasses = `@extends`
    )

  object keys
  protected def properties: List[Property] = List()

  trait Properties {}

  def as[T](range: ClassType[T]): TypedProperty[T] = property.as(range)
  def +[T](range: ClassType[T]): TypedProperty[T]  = property.as(range)
}
