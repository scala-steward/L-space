package lspace.structure

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

  def classtype = ontology

  lazy val ontology: Ontology = {
    val ontology = new Ontology(
      iri,
      iris,
      _properties = () => properties,
      label = Map("en" -> label) ++ labels,
      comment = Map("en" -> comment).filter(_._2.nonEmpty) ++ comments,
      _extendedClasses = `@extends`,
      base = base
    )
    Ontology.ontologies.byIri.getOrElseUpdate(ontology.iri, ontology)
    ontology
  }

  def keys: Object
  def properties: List[Property] = List()

  trait Properties {}
}
