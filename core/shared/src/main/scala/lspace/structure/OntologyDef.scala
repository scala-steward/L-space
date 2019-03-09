package lspace.structure

import monix.eval.Coeval

object OntologyDef {
  implicit def oDefToOntology[T <: OntologyDef](df: T): Ontology = df.ontology
}

/**
  *
  * @param iri
  * @param iris
  * @param label a name for the Ontology in english
  * @param comment a description for the ontology in english
  * @param `@extends` a parent ontology
  * @param base
  */
abstract class OntologyDef(
    iri: String,
    iris: Set[String] = Set(),
    label: String, //move to union types (e.g. String | Map[String, String]) once available (dotty?)
    comment: String = "", //move to union types (e.g. String | Map[String, String]) once available (dotty?)
    `@extends`: () => List[Ontology] = () => List(),
    base: Option[String] = None,
    labels: Map[String, String] = Map(),
    comments: Map[String, String] = Map())
    extends ClassTypeDef[Ontology] {

  def label0    = label
  def comment0  = comment
  def classtype = ontology
  def _extends  = `@extends`

  lazy val ontology: Ontology = {
    val ontology = new Ontology(
      iri,
      iris
//      _properties = () => properties,
//      labelMap = Map("en" -> label0) ++ labels,
//      commentMap = Map("en" -> comment0).filter(_._2.nonEmpty) ++ comments,
//      _extendedClasses = `@extends`,
    ) {
      labelMap = Map("en" -> label0) ++ labels
      commentMap = Map("en" -> comment0).filter(_._2.nonEmpty) ++ comments
      extendedClassesList = Coeval.delay(_extends()).memoizeOnSuccess
      propertiesList = Coeval.delay(properties0.toSet).memoizeOnSuccess
    }
    Ontology.ontologies.byIri.getOrElseUpdate(ontology.iri, ontology)
  }

  def keys: Object
  def properties: List[Property] = List()
  def properties0                = properties

  trait Properties {}
}
