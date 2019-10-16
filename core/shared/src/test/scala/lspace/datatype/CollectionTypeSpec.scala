package lspace.datatype

import lspace.Label.D._
import lspace.structure.{Ontology, Property}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CollectionTypeSpec extends AnyWordSpec with Matchers {

  "build a CollectionType from an complex iri (e.g. @list(@class+@property+@int)" should {
    "handle @list(@class)" in {
      CollectionType.get("@list(@class)") shouldBe Some(`@list`(Ontology.ontology))
    }
    "handle @list()" in { CollectionType.get("@list()") shouldBe Some(`@list`()) }
    "handle @list" in { CollectionType.get("@list") shouldBe Some(`@list`()) }
    "handle @list(@double)" in { CollectionType.get("@list(@double)") shouldBe Some(`@list`(DoubleType.datatype)) }
    "handle @tuple" in { CollectionType.get("@tuple") shouldBe Some(TupleType.datatype) }
    "handle @tuple(@int)(@double)" in {
      CollectionType.get("@tuple(@int)(@double)") shouldBe Some(`@tuple`(`@int` :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@int))(@double)" in {
      CollectionType.get("@tuple(@list(@int))(@double)") shouldBe Some(
        `@tuple`(`@list`(`@int`) :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@geoline))(@double)" in {
      CollectionType.get("@tuple(@list(@geoline))(@double)") shouldBe Some(
        `@tuple`(`@list`(`@geoline`) :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@string))(@double)" in {
      CollectionType.get("@tuple(@list(@string))(@double)") shouldBe Some(
        `@tuple`(`@list`(`@string`) :: `@double` :: Nil map (Some(_))))
    }
    "handle @tuple(@int)(@double)(@date)" in {
      CollectionType.get("@tuple(@int)(@double)(@date)") shouldBe Some(
        `@tuple`(`@int` :: `@double` :: `@date` :: Nil map (Some(_))))
    }
    "handle @tuple(@list(@string))(@list(@geoline))(@list(@string))" in {
      CollectionType.get("@tuple(@list(@string))(@list(@geoline))(@list(@string))") shouldBe Some(
        `@tuple`(`@list`(`@string`) :: `@list`(`@geoline`) :: `@list`(`@string`) :: Nil map (Some(_))))
    }
    "handle @map(@double)(@map(@int)(@string))" in {

      CollectionType.get("@map(@double)(@map(@int)(@string))") shouldBe Some(
        `@map`(DoubleType.datatype, `@map`(IntType.datatype, TextType.datatype)))
    }
    "handle @map(@int)(@string)" in {
      CollectionType.get("@map(@int)(@string)") shouldBe Some(`@map`(IntType.datatype, TextType.datatype))
    }
  }
}
