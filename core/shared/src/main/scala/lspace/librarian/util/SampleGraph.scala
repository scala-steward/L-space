package lspace.librarian.util

import java.time.LocalDate

import lspace.librarian.structure.{Graph, Node, Property, TypedProperty}
import lspace.types.vector.Point
import lspace.librarian.datatype._
import lspace.librarian.structure.OntologyDef
import lspace.librarian.structure.PropertyDef

object SampleGraph {

  object Place extends OntologyDef("https://schema.org/Place", label = "Place") {
    object keys {
      lazy val name                              = SampleGraph.properties.name
      lazy val nameString: TypedProperty[String] = SampleGraph.properties.nameString
      lazy val geo                               = SampleGraph.properties.geo
      lazy val geoPoint: TypedProperty[Point]    = SampleGraph.properties.geoPoint
    }
    override lazy val properties: List[Property] = keys.name.property :: keys.geo.property :: Nil
    trait Properties {
      lazy val name = keys.name
      lazy val geo  = keys.geo
    }
  }
  object Person extends OntologyDef("https://schema.org/Person", label = "Person") {
    object keys {
      lazy val name                                         = SampleGraph.properties.name
      lazy val nameString: TypedProperty[String]            = SampleGraph.properties.nameString
      lazy val birthDate                                    = SampleGraph.properties.birthDate
      lazy val birthDateLocalDate: TypedProperty[LocalDate] = SampleGraph.properties.birthDateLocalDate
      lazy val birthPlace                                   = SampleGraph.properties.birthPlace
      lazy val birthPlaceLocalDate: TypedProperty[Node]     = SampleGraph.properties.birthPlacePlace
      lazy val balance                                      = SampleGraph.properties.balance
      lazy val balanceDouble: TypedProperty[Double]         = SampleGraph.properties.balanceDouble
      lazy val rate                                         = SampleGraph.properties.rate
      lazy val rateInt: TypedProperty[Int]                  = SampleGraph.properties.rateInt
      lazy val knows                                        = SampleGraph.properties.knows
      lazy val knowsPerson: TypedProperty[Node]             = SampleGraph.properties.knowsPerson
    }
    override lazy val properties: List[Property] = keys.name.property :: keys.birthDate.property ::
      keys.birthPlace.property :: keys.balance.property :: keys.rate.property :: keys.knows.property :: Nil
    trait Properties {
      lazy val name       = keys.name
      lazy val birthDate  = keys.birthDate
      lazy val birthPlace = keys.birthPlace
      lazy val balance    = keys.balance
      lazy val rate       = keys.rate
      lazy val knows      = keys.knows
    }
  }
  object ontologies {
    val place  = Place.ontology
    val person = Person.ontology
  }
  object properties {
    object name extends PropertyDef("name", label = "name", `@range` = () => TextType.datatype :: Nil)
    lazy val nameString: TypedProperty[String] = name as TextType.datatype
    object geo
        extends PropertyDef("https://schema.org/geo", label = "geo", `@range` = () => GeopointType.datatype :: Nil)
    lazy val geoPoint: TypedProperty[Point] = geo as GeopointType.datatype
    object birthDate
        extends PropertyDef("https://schema.org/birthDate",
                            label = "birthDate",
                            `@range` = () => LocalDateType.datatype :: Nil)
    lazy val birthDateLocalDate: TypedProperty[LocalDate] = birthDate as LocalDateType.datatype
    object birthPlace
        extends PropertyDef("https://schema.org/birthPlace",
                            label = "birthPlace",
                            `@range` = () => Place.ontology :: Nil)
    lazy val birthPlacePlace: TypedProperty[Node] = birthPlace as Place.ontology
    object balance extends PropertyDef("balance", label = "balance", `@range` = () => DoubleType.datatype :: Nil)
    lazy val balanceDouble: TypedProperty[Double] = balance as DoubleType.datatype
    object rate extends PropertyDef("rate", label = "rate", `@range` = () => IntType.datatype :: Nil)
    lazy val rateInt: TypedProperty[Int] = rate as IntType.datatype
    object knows
        extends PropertyDef("https://schema.org/knows", label = "knows", `@range` = () => Person.ontology :: Nil)
    lazy val knowsPerson: TypedProperty[Node] = knows as Person.ontology
  }
  object namespaces {
    case class NS(iri: String) {
      def /(suffix: String) = iri + "/" + suffix
    }
    val schema = NS("https://schema.org")
  }

  /**
    * Data generated at https://www.generatedata.com/
    * @param graph
    * @return object of places and persons
    */
  def loadSocial(graph: Graph) = new {

    val places = new {
      val SanJosédeMaipo = new {
        val place = graph + ontologies.place
        val id    = place --- Property.default.`@id` --> (graph.iri + "/place/123")
        val name  = place --- "name" --> "San José de Maipo"
        val geo   = place --- properties.geo --> Point(72.0403, 60.90879)
      }
      val CrystalSprings = new {
        val place = graph + ontologies.place
        place --- Property.default.`@id` --> (graph.iri + "/place/12345")
        val name = place --- "name" --> "Crystal Springs"
        val geo  = place --- properties.geo --> Point(-48.4046, 175.87173)
      }
      val Haridwar = new {
        val place = graph + ontologies.place
        val id    = place --- Property.default.`@id` --> (graph.iri + "/place/345")
        val name  = place --- "name" --> "Haridwar"
        val geo   = place --- properties.geo --> Point(89.45136, 88.01204)
      }
      val Talca = new {
        val place = graph + ontologies.place
        val id    = place --- Property.default.`@id` --> (graph.iri + "/place/34567")
        val name  = place --- "name" --> "Talca"
        val geo   = place --- properties.geo --> Point(74.32746, -45.06438)
      }
    }

    val persons = new {
      val Yoshio = new {
        val person     = graph + ontologies.person
        val id         = person --- Property.default.`@id` --> (graph.iri + "/person/123")
        val name       = person --- properties.name --> "Yoshio" //relation can be a string
        val birthdate  = person --- properties.birthDate --> LocalDate.parse("1996-08-18")
        val birthPlace = person --- properties.birthPlace --> places.CrystalSprings.place
        val balance    = person --- properties.balance --> 10.34
        val rate       = person --- properties.rate --> 4
      }
      val Levi = new {
        val person     = graph + ontologies.person
        val id         = person --- Property.default.`@id` --> (graph.iri + "/person/12345")
        val name       = person --- "name" --> "Levi" //relation can be a Property-object
        val birthdate  = person --- namespaces.schema / "birthDate" --> LocalDate.parse("2008-12-20")
        val birthPlace = person --- namespaces.schema / "birthPlace" --> places.CrystalSprings.place
        val balance    = person --- properties.balance --> -245.05
        val rate       = person --- properties.rate --> 2
      }
      val Gray = new {
        val person     = graph + ontologies.person
        val id         = person --- Property.default.`@id` --> (graph.iri + "/person/345")
        val name       = person --- "name" --> "Gray"
        val birthdate  = person --- namespaces.schema / "birthDate" --> LocalDate.parse("1997-04-10")
        val birthPlace = person --- namespaces.schema / "birthPlace" --> places.Haridwar.place
        val balance    = person --- properties.balance --> 2230.30
        val rate       = person --- properties.rate --> 1
      }
      val Kevin = new {
        val person     = graph + ontologies.person
        val id         = person --- Property.default.`@id` --> (graph.iri + "/person/34567")
        val name       = person --- "name" --> "Kevin"
        val birthdate  = person --- namespaces.schema / "birthDate" --> LocalDate.parse("2008-11-30")
        val birthPlace = person --- namespaces.schema / "birthPlace" --> places.SanJosédeMaipo.place
        val balance    = person --- properties.balance --> 500.50
        val rate       = person --- properties.rate --> 2
      }
      val Stan = new {
        val person     = graph + ontologies.person
        val id         = person --- Property.default.`@id` --> (graph.iri + "/person/567")
        val name       = person --- "name" --> "Stan"
        val birthdate  = person --- namespaces.schema / "birthDate" --> LocalDate.parse("2002-06-13")
        val birthPlace = person --- namespaces.schema / "birthPlace" --> places.SanJosédeMaipo.place
        val balance    = person --- properties.balance --> 300
        val rate       = person --- properties.rate --> 4
      }
      val Garrison = new {
        val person     = graph + ontologies.person
        val id         = person --- Property.default.`@id` --> (graph.iri + "/person/56789")
        val name       = person --- "name" --> "Garrison"
        val birthdate  = person --- namespaces.schema / "birthDate" --> LocalDate.parse("1994-06-18")
        val birthPlace = person --- namespaces.schema / "birthPlace" --> places.Talca.place
      }
      val GarrissonKnownStan  = Garrison.person --- properties.knows --- Stan.person
      val GarrissonKnownKevin = Garrison.person --- properties.knows --- Kevin.person
      val KevinKnownStan      = Kevin.person --- properties.knows --- Stan.person
      val KevinKnownGray      = Kevin.person --- properties.knows --- Gray.person
      val GrayKnowsLevi       = Gray.person --- properties.knows --- Levi.person
      val LeviKnowsYoshio     = Levi.person --- properties.knows --- Yoshio.person
    }
  }
}
