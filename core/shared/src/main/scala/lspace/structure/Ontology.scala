package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.NS
import lspace.datatype.{DataType, IriType, NodeURLType}
import lspace.structure.util.ClassTypeable
import monix.eval.{Coeval, Task}

import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object Ontology {
  lazy val ontology: Ontology =
    Ontology(NS.types.`@class`, iris = Set(NS.types.`@class`, NS.types.rdfsClass, NS.types.schemaClass))
  lazy val unknownOntology: Ontology =
    Ontology("@unknownOntology", iris = Set("@unknownOntology"), extendedClasses = () => List(ontology))

  implicit lazy val urlType: IriType[Ontology] = new IriType[Ontology] {
    val iri: String = NS.types.`@class`
  }

  implicit val defaultOntology: ClassTypeable.Aux[Ontology, Ontology, IriType[Ontology]] =
    new ClassTypeable[Ontology] {
      type C  = Ontology
      type CT = IriType[Ontology]
      def ct: CT = urlType
    }
//  import scala.concurrent.duration._
//  private def build(node: Node): Coeval[Ontology] = {
////    println(s"building ${node.iri}")
//    if (node.hasLabel(unknownOntology).nonEmpty) {
//      Coeval(UnknownOntology(node.iri))
//        .map { p =>
//          scribe.trace(s"builded unknown ontology ${p.iri}"); p
//        }
//        .onErrorHandle { f =>
//          scribe.error("could not build unknown ontology? " + f.getMessage); throw f
//        }
//    } else if (node.hasLabel(ontology).nonEmpty) {
//      Coeval
//        .delay {
////        println(s"build ${node.iri}")
////        println(node.out(Property.default.typed.propertyProperty).map(_.iri))
////        println(node.out(Property.default.`@extends`).map(_.asInstanceOf[Node].iri))
//          val properties0 = Coeval.defer(
//            Coeval
//              .sequence(
//                (node
//                  .out(Property.default.typed.propertyProperty) ++ node
//                  .in(lspace.NS.types.schemaDomainIncludes)
//                  .collect { case node: Node => node }) //.filter(_.labels.contains(Property.ontology))
//                  .map(p => Property.properties.getAndUpdate(p))))
//          val extended = Coeval.defer(
//            Coeval
//              .sequence(
//                node
//                  .out(Property.default.`@extends`)
//                  .headOption
//                  .collect {
//                    case nodes: List[_] =>
//                      nodes.collect {
//                        case node: Node if node.hasLabel(Ontology.ontology).isDefined =>
//                          ontologies
//                            .get(node.iri)
//                            .getOrElse {
//                              ontologies.getAndUpdate(node)
//                            } //orElse???
//                        case iri: String =>
//                          ontologies
//                            .get(iri)
//                            .getOrElse(
//                              throw new Exception("@extends looks like an iri but cannot be wrapped by a property"))
//                      }
//                    case node: Node if node.hasLabel(Ontology.ontology).isDefined =>
//                      List(ontologies.get(node.iri).getOrElse(ontologies.getAndUpdate(node)))
//                  }
//                  .toList
//                  .flatten))
//
////        println(s"got properties and range for ${node.iri}")
//          apply(
//            node.iri,
//            iris = node.iris,
//            properties = () => properties0.value(),
//            label = node
//              .outE(Property.default.typed.labelString)
//              .flatMap { edge =>
//                val l = edge.out(Property.default.typed.languageString)
//                if (l.nonEmpty) l.map(_ -> edge.to.value)
//                else List("en"          -> edge.to.value)
//              }
//              .toMap,
//            comment = node
//              .outE(Property.default.typed.commentString)
//              .flatMap { edge =>
//                val l = edge.out(Property.default.typed.languageString)
//                if (l.nonEmpty) l.map(_ -> edge.to.value)
//                else List("en"          -> edge.to.value)
//              }
//              .toMap,
//            extendedClasses = () => extended.value(),
//            base = node.out(Property.default.typed.baseString).headOption
//          )
//        }
//        .memoizeOnSuccess
//        .map { p =>
//          scribe.trace(s"builded ontology ${p.iri}"); p
//        }
//        .onErrorHandle { f =>
//          scribe.error(f.getMessage); throw f
//        }
//    } else {
//      //      new Exception(s"${node.iri} with id ${node.id} is not an ontology, labels: ${node.labels.map(_.iri)}")
//      //        .printStackTrace()
//      scribe.warn(s"could not (yet) build ${node.iri}")
//      Coeval.raiseError(
//        new Exception(s"${node.iri} with id ${node.id} ${node.outE(Property.default.`@id`).head.to.id} " +
//          s"${node.graph.values.hasId(node.outE(Property.default.`@id`).head.to.id).isDefined} is not an ontology, labels: ${node.labels
//            .map(_.iri)}"))
//    }
//  }

  object ontologies {
    object default {
      lazy val ontologies = List(ontology, Property.ontology, DataType.ontology) //::: Step.steps.map(_.ontology)
      if (ontologies.size > 99) throw new Exception("extend default-ontology-id range!")
      val byId    = (200l to 200l + ontologies.size - 1 toList).zip(ontologies).toMap
      val byIri   = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> p) }.toMap
      val idByIri = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> id) }.toMap

      lspace.librarian.traversal.Step.steps
        .map(_.ontology)
      lspace.librarian.logic.predicate.P.predicates
        .map(_.ontology)
    }
    private[lspace] val byIri: concurrent.Map[String, Ontology] =
      new ConcurrentHashMap[String, Ontology]().asScala
//    private[lspace] val building: concurrent.Map[String, Coeval[Ontology]] =
//      new ConcurrentHashMap[String, Coeval[Ontology]]().asScala

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
    def getOrCreate(iri: String, iris: Set[String]): Ontology = get(iri, iris).getOrElse {
      synchronized {
        get(iri, iris).getOrElse {
          val ontology = new Ontology(iri, iris + iri)
          ontology.iris.foreach(byIri.update(_, ontology))
          ontology
        }
      }
    }
    def getAndUpdate(node: Node): Ontology = {
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
        .out(Property.default.typed.propertyProperty) ++ node
        .in(lspace.NS.types.schemaDomainIncludes)
        .collect { case node: Node => node })
        .filter(_.labels.contains(Property.ontology))
        .map(Property.properties.getAndUpdate)

      ontology.extendedClasses ++ node
        .out(Property.default.`@extends`)
        .headOption
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

//    def cache(ontology: Ontology): Unit = {
//      byIri += ontology.iri -> ontology
//      ontology.iris.foreach { iri =>
//        ontologies.byIri += iri -> ontology
//      }
//    }
    def cached(long: Long): Option[Ontology] = default.byId.get(long)
//    def cached(iri: String): Option[Ontology] = default.byIri.get(iri).orElse(byIri.get(iri))

//    def remove(iri: String): Unit = byIri.remove(iri)
  }

  private[structure] def apply(iri: String,
                               iris: Set[String],
                               properties: () => List[Property] = () => List(),
                               label: Map[String, String] = Map(),
                               comment: Map[String, String] = Map(),
                               extendedClasses: () => List[Ontology] = () => List(),
                               base: Option[String] = None): Ontology = {

    def label0           = label
    def comment0         = comment
    def properties0      = properties
    def extendedClasses0 = extendedClasses

    new Ontology(iri, iris) {
      labelMap = label0
      commentMap = comment0
      extendedClassesList = Coeval.delay(extendedClasses0()).memoizeOnSuccess
      propertiesList = Coeval.delay(properties0().toSet).memoizeOnSuccess
    }
  }

  def apply(iri: String): Ontology = Ontology.ontologies.getOrCreate(iri, Set())
}

/**
  *
  * @param iri
  * @param iris
  */
class Ontology(val iri: String,
               val iris: Set[String] = Set()
//               protected val _properties: () => List[Property] = () => List(),
//               protected var labelMap: Map[String, String] = Map(),
//               protected var commentMap: Map[String, String] = Map(),
//               protected val _extendedClasses: () => List[Ontology] = () => List(),
//               val base: Option[String] = None
) extends ClassType[Node] { self =>

//  type Out = Node
//  type CT  = Ontology

  protected var extendedClassesList
    : Coeval[List[Ontology]] = Coeval.now(List()).memoizeOnSuccess //_extendedClasses().filterNot(_.`extends`(this))
  object extendedClasses {
    def apply(): List[Ontology] = extendedClassesList.value()
    def all(): Set[Ontology]    = extendedClasses().toSet ++ extendedClasses().flatMap(_.extendedClasses.all())
    def apply(iri: String): Boolean =
      extendedClassesList().exists(_.iris.contains(iri)) || extendedClassesList().exists(_.extendedClasses(iri))

    def +(parent: Ontology): this.type = this.synchronized {
      if (!parent.`@extends`(self))
        extendedClassesList = extendedClassesList.map(_ :+ parent).map(_.distinct).memoizeOnSuccess
      else scribe.warn(s"$iri cannot extend ${parent.iri} as ${parent.iri} already extends $iri direct or indirect")
      this
    }
    def ++(parent: Iterable[Ontology]): this.type = this.synchronized {
      parent.foreach(this.+)
      this
    }
    def -(parent: Ontology): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map(_.filterNot(_ == parent)).memoizeOnSuccess
      this
    }
    def --(parent: Iterable[Ontology]): this.type = this.synchronized {
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
