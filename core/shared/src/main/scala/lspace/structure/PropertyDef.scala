package lspace.structure

import monix.eval.Coeval

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
                           `@extends`: () => List[Property] = () => List(),
                           labels: Map[String, String] = Map(),
                           comments: Map[String, String] = Map())
    extends ClassTypeDef[Property] {

  def label0    = label
  def comment0  = comment
  def classtype = property
  def _range    = `@range`
  def _extends  = `@extends`

  lazy val property: Property = {
    val property = new Property(
      iri,
      iris,
//      _range = `@range`,
//      containers = container,
//      _properties = () => properties,
//      labelMap = Map("en" -> label) ++ labels,
//      commentMap = Map("en" -> comment).filter(_._2.nonEmpty) ++ comments
//      _extendedClasses = `@extends`
    ) {
      labelMap = Map("en" -> label0) ++ labels
      commentMap = Map("en" -> comment0).filter(_._2.nonEmpty) ++ comments
      rangeList = Coeval.delay(_range()).memoizeOnSuccess
      extendedClassesList = Coeval.delay(_extends()).memoizeOnSuccess
      propertiesList = Coeval.delay(properties0.toSet).memoizeOnSuccess
    }
    Property.properties.byIri.getOrElseUpdate(property.iri, property)
  }

  object keys
  protected def properties: List[Property] = List()
  private def properties0                  = properties

  trait Properties {}

  def as[T](range: ClassType[T]): TypedProperty[T] = property.as(range)
  def +[T](range: ClassType[T]): TypedProperty[T]  = property.as(range)
}
