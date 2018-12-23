package lspace.librarian.util

import java.time.LocalDate

import lspace.librarian.structure.{DataType, Graph, Ontology, Property}
import lspace.types.vector.Point
import lspace.NS.types

object SampleGraph {

  object ontologies {
    val place  = Ontology("httsp://schema.org/Place")
    val person = Ontology("https://schema.org/Person")
  }
  object properties {
    val name       = Property("name")
    val geo        = Property("https://schema.org/geo")
    val birthDate  = Property("https://schema.org/birthDate")
    val birthPlace = Property("https://schema.org/birthPlace")
    val balance    = Property("balance")
    val rate       = Property("rate")
    val knows      = Property("https://schema.org/knows", containers = List(types.`@listset`))
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
    */
  def loadSocial(graph: Graph): Unit = {

    object places {
      val SanJosédeMaipo = {
        val place = graph + ontologies.place
        place --- Property.default.`@id` --> "place-san_jose_de_maipo"
        place --- "name" --> "San José de Maipo"
        place --- properties.geo --> Point(72.0403, 60.90879)
        place
      }
      val CrystalSprings = {
        val place = graph + ontologies.place
        place --- Property.default.`@id` --> "place-crystal_springs"
        place --- "name" --> "Crystal Springs"
        place --- properties.geo --> Point(-48.4046, 175.87173)
        place
      }
      val Haridwar = {
        val place = graph + ontologies.place
        place --- Property.default.`@id` --> "place-haridwar"
        place --- "name" --> "Haridwar"
        place --- properties.geo --> Point(89.45136, 88.01204)
        place
      }
      val Talca = {
        val place = graph + ontologies.place
        place --- Property.default.`@id` --> "place-talca"
        place --- "name" --> "Talca"
        place --- properties.geo --> Point(74.32746, -45.06438)
        place
      }
    }
    val Yoshio = {
      val person = graph + ontologies.person
      person --- Property.default.`@id` --> "person-yoshio"
      person --- properties.name --> "Yoshio" //relation can be a string
      person --- properties.birthDate --> LocalDate.parse("1996-08-18")
      person --- properties.birthPlace --> places.CrystalSprings
      person --- properties.balance --> 10.34
      person --- properties.rate --> 4
      person
    }
    val Levi = {
      val person = graph + ontologies.person
      person --- Property.default.`@id` --> "person-levi"
      person --- "name" --> "Levi" //relation can be a Property-object
      person --- namespaces.schema / "birthDate" --> LocalDate.parse("2008-12-20")
      person --- namespaces.schema / "birthPlace" --> places.CrystalSprings
      person --- properties.balance --> -245.05
      person --- properties.rate --> 2
      person
    }
    val Gray = {
      val person = graph + ontologies.person
      person --- Property.default.`@id` --> "person-gray"
      person --- "name" --> "Gray"
      person --- namespaces.schema / "birthDate" --> LocalDate.parse("1997-04-10")
      person --- namespaces.schema / "birthPlace" --> places.Haridwar
      person --- properties.balance --> 2230.30
      person --- properties.rate --> 1
      person
    }
    val Kevin = {
      val person = graph + ontologies.person
      person --- Property.default.`@id` --> "person-kevin"
      person --- "name" --> "Kevin"
      person --- namespaces.schema / "birthDate" --> LocalDate.parse("2008-11-30")
      person --- namespaces.schema / "birthPlace" --> places.SanJosédeMaipo
      person --- properties.balance --> 500.50
      person --- properties.rate --> 2
      person
    }
    val Stan = {
      val person = graph + ontologies.person
      person --- Property.default.`@id` --> "person-stan"
      person --- "name" --> "Stan"
      person --- namespaces.schema / "birthDate" --> LocalDate.parse("2002-06-13")
      person --- namespaces.schema / "birthPlace" --> places.SanJosédeMaipo
      person --- properties.balance --> 300
      person --- properties.rate --> 4
      person
    }
    val Garrison = {
      val person = graph + ontologies.person
      person --- Property.default.`@id` --> "person-garrisson"
      person --- "name" --> "Garrison"
      person --- namespaces.schema / "birthDate" --> LocalDate.parse("1994-06-18")
      person --- namespaces.schema / "birthPlace" --> places.Talca
      person
    }

    Garrison --- properties.knows --- Stan
    Garrison --- properties.knows --- Kevin
    Kevin --- properties.knows --- Stan
    Kevin --- properties.knows --- Gray
    Gray --- properties.knows --- Levi
    Levi --- properties.knows --- Yoshio
  }
}
