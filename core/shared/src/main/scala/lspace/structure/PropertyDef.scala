package lspace.structure

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
                           `@range`: => List[ClassType[_]] = List(),
                           `@extends`: => List[Property] = List(),
                           labels: Map[String, String] = Map(),
                           comments: Map[String, String] = Map())
    extends ClassTypeDef[Property] {

  def classtype = property

  val property: Property = Property.properties.getOrCreate(iri, iris)
  property.label ++ Map("en"   -> label).filter(_._2.nonEmpty) ++ labels.filter(_._2.nonEmpty)
  property.comment ++ Map("en" -> comment).filter(_._2.nonEmpty) ++ comments.filter(_._2.nonEmpty)
  property.range ++ `@range`
  property.extendedClasses ++ `@extends`
  property.properties ++ properties.toSet

//  def keys: Object               = new {}
  lazy val properties: List[Property] = List()
//  private def properties0        = properties

  trait Properties {}

  def as[T](range: ClassType[T]): TypedProperty[T] = property.as(range)
//  def +[T](range: ClassType[T]): TypedProperty[T]  = property.as(range)
}
