package lspace.datatype

import lspace.Label.D._
import lspace.structure.{Ontology, Property}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CollectionTypeSpec extends AnyWordSpec with Matchers {

  "CollectionType" should {
    "build a CollectionType from an complex iri (e.g. @list(@class+@property+@int)" in {
      CollectionType.get("@list(@class)") shouldBe Some(ListType(Ontology.ontology))
      CollectionType.get("@list()") shouldBe Some(ListType())
      CollectionType.get("@list()") shouldBe Some(ListType.datatype)
      CollectionType.get("@list") shouldBe Some(ListType.datatype)
      CollectionType.get("@list(@double)") shouldBe Some(ListType(DoubleType.datatype))
      CollectionType.get("@tuple") shouldBe Some(TupleType.datatype)
      CollectionType.get("@tuple(@int)(@double)") shouldBe Some(TupleType(`@int` :: `@double` :: Nil map (Some(_))))
      CollectionType.get("@tuple(@int)(@double)(@date)") shouldBe Some(
        TupleType(`@int` :: `@double` :: `@date` :: Nil map (Some(_))))
      CollectionType.get("@map(@int)(@string)") shouldBe Some(MapType(IntType.datatype, TextType.datatype))
      CollectionType.get("@map(@double)(@map(@int)(@string))") shouldBe Some(
        MapType(DoubleType.datatype, MapType(IntType.datatype, TextType.datatype)))
    }
  }
}
