package lspace.structure

import lspace.datatype.DataType

import monix.eval.Task

abstract class Classtypes(val graph: NameSpaceGraph) {
  import graph._

  def get(iri: String): Task[Option[ClassType[_]]] =
    datatypes
      .get(iri)
      .flatMap { dt =>
        if (dt.isDefined) Task.now(dt)
        else
          properties.get(iri).flatMap { pt =>
            if (pt.isDefined) Task.now(pt)
            else ontologies.get(iri)
          }
      }

  def get(node: Node): ClassType[_] = node.labels match {
    case l if l.contains(DataType.ontology) =>
      datatypes.cached(iri).getOrElse {
        DataType.datatypes.getAndUpdate(node)
      }
    case l if l.contains(Property.ontology) =>
      properties.cached(iri).getOrElse {
        Property.properties.getAndUpdate(node)
      }
    case l if l.contains(Ontology.ontology) =>
      ontologies.cached(iri).getOrElse {
        Ontology.ontologies.getAndUpdate(node)
      }
    case _ =>
      throw new Exception(s"could not find class-type ${node.iri}")
  }

  def cached(iri: String): Option[ClassType[_]] =
    datatypes
      .cached(iri)
      .orElse(ontologies.cached(iri))
      .orElse(properties.cached(iri))

  def store[CT <: ClassType[_]](classType: CT): Task[Node] = classType match {
    case ontology: Ontology    => ontologies.store(ontology)
    case property: Property    => properties.store(property)
    case datatype: DataType[_] => datatypes.store(datatype)
    case _ => throw new Exception(s"unexpected type ${classType.getClass.getSimpleName}")
  }
}
