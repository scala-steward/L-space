package lspace.librarian.structure

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
abstract class OntologyDef(iri: String,
                           iris: Set[String] = Set(),
                           label: String,
                           comment: String = "",
                           `@extends`: () => List[Ontology] = () => List(),
                           base: Option[String] = None)
    extends ClassTypeDef[Ontology] {

  def classtype = ontology

  lazy val ontology: Ontology =
    new Ontology(iri,
                 iris,
                 _properties = () => properties,
                 label = Map("en"   -> label),
                 comment = Map("en" -> comment),
                 _extendedClasses = `@extends`,
                 base = base)

  def keys: Object
  def properties: List[Property] = List()

  trait Properties {}
}
