package lspace.structure

import monix.eval.Coeval

object OntologyDef {
  implicit def oDefToOntology[T <: OntologyDef](df: T): Ontology = df.ontology

//  object defs {
//    private var list
//    : Coeval[List[OntologyDef]] = Coeval.now(List()).memoizeOnSuccess
//
//    def get: List[OntologyDef]] =
//  }
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
    `@extends`: => List[Ontology] = List(),
    base: Option[String] = None,
    val labels: Map[String, String] = Map(),
    val comments: Map[String, String] = Map())
    extends ClassTypeDef[Ontology] {

  def classtype = ontology

  val ontology: Ontology = Ontology.ontologies.getOrCreate(iri, iris)
  ontology.label ++ (Map("en"   -> label).filter(_._2.nonEmpty) ++ labels.filter(_._2.nonEmpty))
  ontology.comment ++ (Map("en" -> comment).filter(_._2.nonEmpty) ++ comments.filter(_._2.nonEmpty))
  ontology.extendedClasses.++(`@extends`)
  ontology.properties ++ properties.toSet

//  def keys: Object               = new {}
  lazy val properties: List[Property] = List()
//  private def properties0        = properties

  trait Properties {}
}
