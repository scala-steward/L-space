package lspace.datatype

import lspace.Label.D._
import lspace.structure.{Ontology, Property}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CollectionTypeSpec extends AnyWordSpec with Matchers {

  "build a CollectionType from an complex iri (e.g. @list(@class+@property+@int)" should {
    "handle @list(@class)" in {
      CollectionType.get("@list(@class)") shouldBe Some(ListType(Ontology.ontology))
    }
    "handle @list()" in { CollectionType.get("@list()") shouldBe Some(ListType()) }
    "handle @list" in { CollectionType.get("@list") shouldBe Some(ListType.datatype) }
    "handle @list(@double)" in { CollectionType.get("@list(@double)") shouldBe Some(ListType(DoubleType.datatype)) }
    "handle @tuple" in { CollectionType.get("@tuple") shouldBe Some(TupleType.datatype) }
    "handle @tuple(@int)(@double)" in {
      CollectionType.get("@tuple(@int)(@double)") shouldBe Some(TupleType(`@int` :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@int))(@double)" in {
      CollectionType.get("@tuple(@list(@int))(@double)") shouldBe Some(
        TupleType(ListType(`@int`) :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@geoline))(@double)" in {
      CollectionType.get("@tuple(@list(@geoline))(@double)") shouldBe Some(
        TupleType(ListType(`@geoline`) :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@string))(@double)" in {
      CollectionType.get("@tuple(@list(@string))(@double)") shouldBe Some(
        TupleType(ListType(`@string`) :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@int)(@double)(@date)" in {
      CollectionType.get("@tuple(@int)(@double)(@date)") shouldBe Some(
        TupleType(`@int` :: `@double` :: `@date` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@string))(@list(@geoline))(@list(@string))" in {
      CollectionType.get("@tuple(@list(@string))(@list(@geoline))(@list(@string))") shouldBe Some(
        TupleType(ListType(`@string`) :: ListType(`@geoline`) :: ListType(`@string`) :: Nil map (Some(_))))
    }
    "handle @map(@double)(@map(@int)(@string))" in {

      CollectionType.get("@map(@double)(@map(@int)(@string))") shouldBe Some(
        MapType(DoubleType.datatype, MapType(IntType.datatype, TextType.datatype)))
    }
    "handle @map(@int)(@string)" in {
      CollectionType.get("@map(@int)(@string)") shouldBe Some(MapType(IntType.datatype, TextType.datatype))
    }
  }
}
