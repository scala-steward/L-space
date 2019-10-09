package lspace.util

import java.time.LocalDate

import lspace.datatype._
import lspace.structure._
import lspace.types.geo.Point
import monix.eval.Task

object SampleGraph {

  object Place extends OntologyDef("https://example.org/Place", label = "Place") {
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
  object Person extends OntologyDef("https://example.org/Person", label = "Person") {
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
    object name extends PropertyDef("name", label = "name", `@range` = TextType.datatype :: Nil)
    lazy val nameString: TypedProperty[String] = name as TextType.datatype
    object geo extends PropertyDef("https://example.org/geo", label = "geo", `@range` = GeopointType.datatype :: Nil)
    lazy val geoPoint: TypedProperty[Point] = geo as GeopointType.datatype
    object birthDate
        extends PropertyDef("https://example.org/birthDate",
                            label = "birthDate",
                            `@range` = LocalDateType.datatype :: Nil)
    lazy val birthDateLocalDate: TypedProperty[LocalDate] = birthDate as LocalDateType.datatype
    object birthPlace
        extends PropertyDef("https://example.org/birthPlace", label = "birthPlace", `@range` = Place.ontology :: Nil)
    lazy val birthPlacePlace: TypedProperty[Node] = birthPlace as Place.ontology
    object balance extends PropertyDef("balance", label = "balance", `@range` = DoubleType.datatype :: Nil)
    lazy val balanceDouble: TypedProperty[Double] = balance as DoubleType.datatype
    object rate extends PropertyDef("rate", label = "rate", `@range` = IntType.datatype :: Nil)
    lazy val rateInt: TypedProperty[Int] = rate as IntType.datatype
    object knows extends PropertyDef("https://example.org/knows", label = "knows", `@range` = Person.ontology :: Nil)
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
  def loadSocial(graph: Graph) = {

    for {
      _places <- for {
        _SanJosédeMaipo <- for {
          _place <- graph + ontologies.place
          _id    <- _place --- Property.default.`@id` --> (graph.iri + "/place/123")
          _name  <- _place --- "name" --> "San José de Maipo"
          _geo   <- _place --- properties.geo --> Point(72.0403, 60.90879)
        } yield
          new {
            val place = _place
            val id    = _id
            val name  = _name
            val geo   = _geo
          }
        _CrystalSprings <- for {
          _place <- graph + ontologies.place
          _id    <- _place --- Property.default.`@id` --> (graph.iri + "/place/12345")
          _name  <- _place --- "name" --> "Crystal Springs"
          _geo   <- _place --- properties.geo --> Point(-48.4046, 175.87173)
        } yield
          new {
            val place = _place
            val id    = _id
            val name  = _name
            val geo   = _geo
          }
        _Haridwar <- for {
          _place <- graph + ontologies.place
          _id    <- _place --- Property.default.`@id` --> (graph.iri + "/place/345")
          _name  <- _place --- "name" --> "Haridwar"
          _geo   <- _place --- properties.geo --> Point(89.45136, 88.01204)
        } yield
          new {
            val place = _place
            val id    = _id
            val name  = _name
            val geo   = _geo
          }
        _Talca <- for {
          _place <- graph + ontologies.place
          _id    <- _place --- Property.default.`@id` --> (graph.iri + "/place/34567")
          _name  <- _place --- "name" --> "Talca"
          _geo   <- _place --- properties.geo --> Point(74.32746, -45.06438)
        } yield
          new {
            val place = _place
            val id    = _id
            val name  = _name
            val geo   = _geo
          }
      } yield
        new {
          val SanJosédeMaipo = _SanJosédeMaipo
          val CrystalSprings = _CrystalSprings
          val Haridwar       = _Haridwar
          val Talca          = _Talca
        }
      _persons <- for {
        _Yoshio <- for {
          _person     <- graph + ontologies.person
          _id         <- _person --- Property.default.`@id` --> (graph.iri + "/person/123")
          _name       <- _person --- properties.name --> "Yoshio" //relation can be a string
          _birthdate  <- _person --- properties.birthDate --> LocalDate.parse("1996-08-18")
          _birthPlace <- _person --- properties.birthPlace --> _places.CrystalSprings.place
          _balance    <- _person --- properties.balance --> 10.34
          _rate       <- _person --- properties.rate --> 4
        } yield
          new {
            val person     = _person
            val id         = _id
            val name       = _name
            val birthdate  = _birthdate
            val birthPlace = _birthPlace
            val balance    = _balance
            val rate       = _rate
          }
        _Levi <- for {
          _person     <- graph + ontologies.person
          _id         <- _person --- Property.default.`@id` --> (graph.iri + "/person/12345")
          _name       <- _person --- properties.name --> "Levi" //relation can be a string
          _birthdate  <- _person --- properties.birthDate --> LocalDate.parse("2008-12-20")
          _birthPlace <- _person --- properties.birthPlace --> _places.CrystalSprings.place
          _balance    <- _person --- properties.balance --> -245.05
          _rate       <- _person --- properties.rate --> 2
        } yield
          new {
            val person     = _person
            val id         = _id
            val name       = _name
            val birthdate  = _birthdate
            val birthPlace = _birthPlace
            val balance    = _balance
            val rate       = _rate
          }
        _Gray <- for {
          _person     <- graph + ontologies.person
          _id         <- _person --- Property.default.`@id` --> (graph.iri + "/person/345")
          _name       <- _person --- properties.name --> "Gray" //relation can be a string
          _birthdate  <- _person --- properties.birthDate --> LocalDate.parse("1997-04-10")
          _birthPlace <- _person --- properties.birthPlace --> _places.Haridwar.place
          _balance    <- _person --- properties.balance --> 2230.30
          _rate       <- _person --- properties.rate --> 1
        } yield
          new {
            val person     = _person
            val id         = _id
            val name       = _name
            val birthdate  = _birthdate
            val birthPlace = _birthPlace
            val balance    = _balance
            val rate       = _rate
          }
        _Kevin <- for {
          _person     <- graph + ontologies.person
          _id         <- _person --- Property.default.`@id` --> (graph.iri + "/person/34567")
          _name       <- _person --- properties.name --> "Kevin" //relation can be a string
          _birthdate  <- _person --- properties.birthDate --> LocalDate.parse("2008-11-30")
          _birthPlace <- _person --- properties.birthPlace --> _places.SanJosédeMaipo.place
          _balance    <- _person --- properties.balance --> 500.50
          _rate       <- _person --- properties.rate --> 2
        } yield
          new {
            val person     = _person
            val id         = _id
            val name       = _name
            val birthdate  = _birthdate
            val birthPlace = _birthPlace
            val balance    = _balance
            val rate       = _rate
          }
        _Stan <- for {
          _person     <- graph + ontologies.person
          _id         <- _person --- Property.default.`@id` --> (graph.iri + "/person/567")
          _name       <- _person --- properties.name --> "Stan" //relation can be a string
          _birthdate  <- _person --- properties.birthDate --> LocalDate.parse("2002-06-13")
          _birthPlace <- _person --- properties.birthPlace --> _places.SanJosédeMaipo.place
          _balance    <- _person --- properties.balance --> 300
          _rate       <- _person --- properties.rate --> 4
        } yield
          new {
            val person     = _person
            val id         = _id
            val name       = _name
            val birthdate  = _birthdate
            val birthPlace = _birthPlace
            val balance    = _balance
            val rate       = _rate
          }
        _Garrison <- for {
          _person     <- graph + ontologies.person
          _id         <- _person --- Property.default.`@id` --> (graph.iri + "/person/56789")
          _name       <- _person --- properties.name --> "Garrison" //relation can be a string
          _birthdate  <- _person --- properties.birthDate --> LocalDate.parse("1994-06-18")
          _birthPlace <- _person --- properties.birthPlace --> _places.Talca.place
        } yield
          new {
            val person     = _person
            val id         = _id
            val name       = _name
            val birthdate  = _birthdate
            val birthPlace = _birthPlace
          }
      } yield
        new {
          val Yoshio   = _Yoshio
          val Levi     = _Levi
          val Gray     = _Gray
          val Kevin    = _Kevin
          val Stan     = _Stan
          val Garrison = _Garrison
        }
      _knows <- {
        import _persons._
        for {
          _GarrissonKnownStan  <- Garrison.person --- properties.knows --- Stan.person
          _GarrissonKnownKevin <- Garrison.person --- properties.knows --- Kevin.person
          _KevinKnownStan      <- Kevin.person --- properties.knows --- Stan.person
          _KevinKnownGray      <- Kevin.person --- properties.knows --- Gray.person
          _GrayKnowsLevi       <- Gray.person --- properties.knows --- Levi.person
          _LeviKnowsYoshio     <- Levi.person --- properties.knows --- Yoshio.person
        } yield
          new {
            val GarrissonKnownStan  = _GarrissonKnownStan
            val GarrissonKnownKevin = _GarrissonKnownKevin
            val KevinKnownStan      = _KevinKnownStan
            val KevinKnownGray      = _KevinKnownGray
            val GrayKnowsLevi       = _GrayKnowsLevi
            val LeviKnowsYoshio     = _LeviKnowsYoshio
          }
      }
    } yield
      new {
        val places  = _places
        val persons = _persons
        val knows   = _knows
      }
  }
}
