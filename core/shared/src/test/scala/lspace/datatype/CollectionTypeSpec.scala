package lspace.datatype

import lspace.structure.{Ontology, Property}
import org.scalatest.{Matchers, WordSpec}

class CollectionTypeSpec extends WordSpec with Matchers {

  "CollectionType" should {
    "build a CollectionType from an complex iri (e.g. @list(@class+@property+@int)" in {
      CollectionType.get("@list(@class)") shouldBe Some(ListType(Ontology.ontology))
      CollectionType.get("@list()") shouldBe Some(ListType())
      CollectionType.get("@list()") shouldBe Some(ListType.datatype)
      CollectionType.get("@list") shouldBe Some(ListType.datatype)
      CollectionType.get("@list(@double)") shouldBe Some(ListType(DoubleType.datatype))
      CollectionType.get("@map(@int)(@string)") shouldBe Some(MapType(IntType.datatype, TextType.datatype))
      CollectionType.get("@map(@double)(@map(@int)(@string))") shouldBe Some(
        MapType(DoubleType.datatype, MapType(IntType.datatype, TextType.datatype)))
    }
  }
}
