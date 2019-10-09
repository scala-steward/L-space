package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.NS
import lspace.datatype.{DataType, IriType, NodeURLType}
import lspace.structure.util.ClassTypeable
import monix.eval.{Coeval, Task}

import scala.collection.concurrent
import scala.collection.JavaConverters._

object Ontology {
  lazy val ontology: Ontology = {
    val ontology = new Ontology(NS.types.`@class`,
                                Set(NS.types.`@class`,
                                    NS.types.rdfsClass,
                                    NS.types.schemaClass,
                                    "https://schema.org/Class",
                                    "http://schema.org/Class"))
    ontology.iris.foreach(ontologies.byIri.update(_, ontology))
    ontology
  }
//    Ontology(NS.types.`@class`,
//             iris = Set(NS.types.`@class`,
//                        NS.types.rdfsClass,
//                        NS.types.schemaClass,
//                        "https://schema.org/Class",
//                        "http://schema.org/Class"))
//  lazy val unknownOntology: Ontology =
//    Ontology("@unknownOntology", iris = Set("@unknownOntology"), extendedClasses = List(ontology))

  lazy val empty: Ontology = Ontology("")

  implicit lazy val urlType: IriType[Ontology] = new IriType[Ontology] {
    val iri: String = NS.types.`@class`
  }

  implicit val defaultOntology: ClassTypeable.Aux[Ontology, Ontology, IriType[Ontology]] =
    new ClassTypeable[Ontology] {
      type C  = Ontology
      type CT = IriType[Ontology]
      def ct: CT = urlType
    }

  object ontologies {
    object default {
      val ontologies = List(ontology, Property.ontology, DataType.ontology) //::: Step.steps.map(_.ontology)
      if (ontologies.size > 99) throw new Exception("extend default-ontology-id range!")
      val byId    = (200l to 200l + ontologies.size - 1 toList).zip(ontologies).toMap
      val byIri   = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> p) }.toMap
      val idByIri = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> id) }.toMap
    }
    private[lspace] val byIri: concurrent.Map[String, Ontology] =
      new ConcurrentHashMap[String, Ontology]().asScala

    def all: List[Ontology] = byIri.values.toList.distinct
    def get(iri: String, iris: Set[String] = Set()): Option[Ontology] = {
      val allIris = (iris + iri)
      allIris.flatMap(iri => default.byIri.get(iri).orElse(byIri.get(iri))).toList match {
        case List(ontology) => Some(ontology)
        case Nil            => None
        case ontologies =>
          scribe.warn(
            "It looks like multiple ontologies which have some @id's in common are found, this should not happen...")
          ontologies.headOption
      }
    }
    def getOrCreate(iri: String, iris: Set[String] = Set()): Ontology = get(iri, iris).getOrElse {
      synchronized {
        get(iri, iris).getOrElse {
          val ontology = new Ontology(iri, iris + iri)
          ontology.iris.foreach(byIri.update(_, ontology))
//          ontology.extendedClasses.all() //force eagerly init of extended classes
//          ontology.properties()          //force eagerly init of associated properties
          ontology
        }
      }
    }
    def getAndUpdate(node: Node): Ontology = {
      if (node.hasLabel(Ontology.ontology).isEmpty)
        throw new Exception("cannot create Ontology from node without label @class")
      if (node.iri.isEmpty) throw new Exception("cannot create Ontology with empty iri")
      val ontology = getOrCreate(node.iri, node.iris)

      ontology.label ++ node
        .outE(Property.default.typed.labelString)
        .flatMap { edge =>
          val l = edge.out(Property.default.typed.languageString)
          if (l.nonEmpty) l.map(_ -> edge.to.value)
          else List("en"          -> edge.to.value)
        }
        .toMap
      ontology.comment ++ node
        .outE(Property.default.typed.commentString)
        .flatMap { edge =>
          val l = edge.out(Property.default.typed.commentString)
          if (l.nonEmpty) l.map(_ -> edge.to.value)
          else List("en"          -> edge.to.value)
        }
        .toMap

      ontology.properties ++ (node
        .out(Property.default.typed.propertyProperty)
        .filter(_.out("http://schema.org/supersededBy").isEmpty) ++ node
        .in(lspace.NS.types.schemaDomainIncludes)
        .collect { case node: Node => node })
        .filter(_.labels.contains(Property.ontology))
        .filter(_.out("http://schema.org/supersededBy").isEmpty)
        .map(Property.properties.getAndUpdate)

      ontology.extendedClasses ++ node
        .out(Property.default.`@extends`)
//        .headOption
        .collect {
          case nodes: List[_] =>
            nodes.collect {
              case node: Node if node.hasLabel(Ontology.ontology).isDefined =>
                Ontology.ontologies
                  .get(node.iri, node.iris)
                  .getOrElse {
                    Ontology.ontologies.getAndUpdate(node)
                  } //orElse???
              case iri: String =>
                Ontology.ontologies
                  .get(iri)
                  .getOrElse(throw new Exception("@extends looks like an iri but cannot be wrapped by a property"))
            }
          case node: Node if node.hasLabel(Ontology.ontology).isDefined =>
            List(Ontology.ontologies.get(node.iri, node.iris).getOrElse(Ontology.ontologies.getAndUpdate(node)))
        }
        .toList
        .flatten

      ontology
    }

    def cached(long: Long): Option[Ontology] = default.byId.get(long)
  }

//  private[structure] def apply(iri: String,
//                               iris: Set[String],
//                               properties: () => List[Property] = () => List(),
//                               label: Map[String, String] = Map(),
//                               comment: Map[String, String] = Map(),
//                               extendedClasses: () => List[Ontology] = () => List(),
//                               base: Option[String] = None): Ontology = {
//
//    def label0           = label
//    def comment0         = comment
//    def properties0      = properties
//    def extendedClasses0 = extendedClasses
//
//    new Ontology(iri, iris) {
//      labelMap ++= label0
//      commentMap = comment0
//      extendedClassesList = Coeval.delay(extendedClasses0()).memoizeOnSuccess
//      propertiesList = Coeval.delay(properties0().toSet).memoizeOnSuccess
//    }
//  }

  implicit def apply(iri: String): Ontology = Ontology.ontologies.getOrCreate(iri, Set())
}

/**
  *
  * @param iri
  * @param iris
  */
class Ontology(val iri: String, val iris: Set[String] = Set()) extends ClassType[Node] { self =>

  protected var extendedClassesList
    : Coeval[List[Ontology]] = Coeval(List()).memoizeOnSuccess //_extendedClasses().filterNot(_.`extends`(this))
  object extendedClasses {
    def apply(): List[Ontology] = extendedClassesList.value()
    def all(): Set[Ontology]    = extendedClasses().toSet ++ extendedClasses().flatMap(_.extendedClasses.all())
    def apply(iri: String): Boolean =
      extendedClassesList().exists(_.iris.contains(iri)) || extendedClassesList().exists(_.extendedClasses(iri))

    def +(parent: => Ontology): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map { current =>
        val _parent = parent
        if (!_parent.`@extends`(self))
          (current :+ _parent).distinct
        else {
          scribe.warn(
            s"$iri cannot extend ${_parent.iri} as ${_parent.iri} already extends $iri direct or indirect ${extendedClassesList.value().map(_.iri)}")
          current
        }
      }.memoizeOnSuccess
      this
    }
    def ++(parents: => Iterable[Ontology]): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map { current =>
        (current ++ parents.filterNot(_.`@extends`(self))).distinct
      }.memoizeOnSuccess
      this
    }
    def -(parent: => Ontology): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map(_.filterNot(_ == parent)).memoizeOnSuccess
      this
    }
    def --(parent: => Iterable[Ontology]): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map(_.filterNot(parent.toList.contains)).memoizeOnSuccess
      this
    }
  }

  override def toString: String = s"ontology:$iri"

  override def equals(o: Any): Boolean = o match {
    case p: Ontology => iri == p.iri || iris.contains(p.iri)
    case _           => false
  }

  override def hashCode(): Int = iri.hashCode
}
