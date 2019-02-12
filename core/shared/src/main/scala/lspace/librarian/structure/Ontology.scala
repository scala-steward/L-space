package lspace.librarian.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.NS
import lspace.librarian.datatype.{DataType, IriType, NodeURLType}
import lspace.librarian.process.traversal.helper.ClassTypeable
import monix.eval.{Coeval, Task}

import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object Ontology {
  lazy val ontology: Ontology =
    Ontology(NS.types.`@class`, iris = Set(NS.types.rdfsClass))

  //  lazy val classType: ClassType[Ontology] = ClassType[Ontology](ldcontext.types.CLASS)
  implicit lazy val urlType: IriType[Ontology] = new IriType[Ontology] {
    val iri: String = NS.types.`@class`
  }

  implicit val defaultOntology: ClassTypeable.Aux[Ontology, Node, NodeURLType[Node]] =
    new ClassTypeable[Ontology] {
      type C  = Node
      type CT = NodeURLType[Node]
      def ct: CT = NodeURLType.apply[Node]
    }

//  implicit def iriToCachedOntology(iri: String): Ontology =
//   ontologies.cached(iri).getOrElse(Ontology(iri)) //todo get from remote
//  implicit def ontologyToString(ontology: Ontology): String = ontology.iri

  def build(node: Node): Task[Coeval[Ontology]] = {
    if (node.hasLabel(ontology).nonEmpty) {
      for {
        properties <- Task.gather(node.out(Property.default.typed.propertyProperty).map(Property.build))
        extended <- Task.gather(node.out(Property.default.`@extends`).collect {
          case node: Node => ontologies.getOrConstruct(node.iri)(build(node))
        })
      } yield {
        Coeval(
          _Ontology(node.iri)(
            iris = node.iris,
            _properties = () => properties.map(_.value()),
            label = node
              .outE(Property.default.typed.labelString)
              .flatMap { edge =>
                edge.out(Property.default.typed.languageString).map(_ -> edge.to.value)
              }
              .toMap,
            comment = node
              .outE(Property.default.typed.commentString)
              .flatMap { edge =>
                edge.out(Property.default.typed.languageString).map(_ -> edge.to.value)
              }
              .toMap,
            _extendedClasses = () => extended.map(_.value()),
            base = node.out(Property.default.typed.baseString).headOption
          ))
      }
    } else {
      //      new Exception(s"${node.iri} with id ${node.id} is not an ontology, labels: ${node.labels.map(_.iri)}")
      //        .printStackTrace()
      Task.raiseError(new Exception(s"${node.iri} with id ${node.id} ${node.outE(Property.default.`@id`).head.to.id} " +
        s"${node.graph.values.hasId(node.outE(Property.default.`@id`).head.to.id).isDefined} is not an ontology, labels: ${node.labels
          .map(_.iri)}"))
    }
  }

  object ontologies {
    object default {
      lazy val ontologies = List(ontology, Property.ontology, DataType.ontology) //::: Step.steps.map(_.ontology)
      if (ontologies.size > 99) throw new Exception("extend default-ontology-id range!")
      val byId    = (200l to 200l + ontologies.size - 1 toList).zip(ontologies).toMap
      val byIri   = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> p) }.toMap
      val idByIri = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> id) }.toMap
    }
    private[lspace] val byIri: concurrent.Map[String, Ontology] =
      new ConcurrentHashMap[String, Ontology]().asScala
    private[lspace] val constructing: concurrent.Map[String, Task[Coeval[Ontology]]] =
      new ConcurrentHashMap[String, Task[Coeval[Ontology]]]().asScala

    def get(iri: String): Option[Task[Coeval[Ontology]]] =
      default.byIri
        .get(iri)
        .orElse(byIri.get(iri))
        .map(o => Task.now(Coeval.now(o)))
        .orElse(constructing.get(iri))
    def getOrConstruct(iri: String)(constructTask: Task[Coeval[Ontology]]): Task[Coeval[Ontology]] =
      default.byIri
        .get(iri)
        .map(o => Task.now(Coeval.now(o)))
        .getOrElse(constructing.getOrElseUpdate(
          iri,
          constructTask
            .map(_.memoize)
            .map { o =>
              Task {
                byIri += o.value().iri -> o.value()
                o.value().iris.foreach { iri =>
                  ontologies.byIri += iri -> o.value()
                }
                constructing.remove(iri)
              }.delayExecution(FiniteDuration(1, "s"))
                .runAsyncAndForget(monix.execution.Scheduler.global)
              o
            }
            .memoize
        ))

    def cached(long: Long): Option[Ontology]  = default.byId.get(long)
    def cached(iri: String): Option[Ontology] = default.byIri.get(iri).orElse(byIri.get(iri))

    def remove(iri: String): Unit = byIri.remove(iri)
  }

  def _Ontology(iri: String)(implicit
                             iris: Set[String] = Set(),
                             _properties: () => List[Property] = () => List(),
                             label: Map[String, String] = Map(),
                             comment: Map[String, String] = Map(),
                             _extendedClasses: () => List[Ontology] = () => List(),
                             base: Option[String] = None): Ontology =
    new Ontology(iri, iris, _properties, label, comment, _extendedClasses, base) {}

  def apply(iri: String,
            iris: Set[String] = Set(),
            properties: List[Property] = List(),
            label: Map[String, String] = Map(),
            comment: Map[String, String] = Map(),
            extendedClasses: List[Ontology] = List(),
            base: Option[String] = None): Ontology =
    new Ontology(iri, iris, () => properties, label, comment, () => extendedClasses, base) {}
}

//trait OntologyBase extends ClassType[Node] {
//
//}
//case class OntologyRef(iri: String) extends OntologyBase {
//  override def equals(o: Any): Boolean = o match {
//    case p: OntologyRef => iri == p.iri || iris.contains(p.iri)
//    case _           => false
//  }
//
//  override def hashCode(): Int = iri.hashCode
//
//  override def toString: String = s"ontology:$iri"
//}

/**
  *
  * @param iri
  * @param iris
  * @param _properties common meta-properties
  * @param label a human-readable name
  * @param comment a human-readable description
  * @param _extendedClasses inherited ontologies
  * @param base base-iri of the resource typed with this ontology
  */
class Ontology(val iri: String,
               val iris: Set[String] = Set(),
               protected val _properties: () => List[Property] = () => List(),
               val label: Map[String, String] = Map(),
               val comment: Map[String, String] = Map(),
               protected val _extendedClasses: () => List[Ontology] = () => List(),
               val base: Option[String] = None)
    extends ClassType[Node] {

//  type Out = Node
//  type CT  = Ontology

  override lazy val extendedClasses: List[Ontology] = _extendedClasses()

  override def toString: String = s"ontology:$iri"

  override def equals(o: Any): Boolean = o match {
    case p: Ontology => iri == p.iri || iris.contains(p.iri)
    case _           => false
  }

  override def hashCode(): Int = iri.hashCode
}
