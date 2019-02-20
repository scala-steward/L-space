package lspace.datatype

import lspace.structure.{Ontology, Property}
import org.scalatest.{Matchers, WordSpec}

class CollectionTypeSpec extends WordSpec with Matchers {

  "CollectionType" should {
    "build a CollectionType from an complex iri (e.g. @list(@class+@property+@int)" in {
      CollectionType.get("@list(@class+@property+@int)") shouldBe Some(
        ListType(List(Ontology.ontology, Property.ontology, IntType.datatype)))
      CollectionType.get("@list(@class)") shouldBe Some(ListType(List(Ontology.ontology)))
      CollectionType.get("@list()") shouldBe Some(ListType(List()))
      CollectionType.get("@list()") shouldBe Some(ListType.datatype)
      CollectionType.get("@list") shouldBe Some(ListType.datatype)
      CollectionType.get("@list(@class+@double+@int)") shouldBe Some(
        ListType(List(Ontology.ontology, DoubleType.datatype, IntType.datatype)))
      CollectionType.get("@list(@class+@property+@int)") should not be Some(
        ListType(List(Ontology.ontology, Property.ontology, DoubleType.datatype)))
      CollectionType.get("@map(@int+@double)(@string)") shouldBe Some(
        MapType(List(IntType.datatype, DoubleType.datatype), List(TextType.datatype)))
      CollectionType.get("@map(@int+@double)(@map(@int+@double)(@string))") shouldBe Some(
        MapType(List(IntType.datatype, DoubleType.datatype),
                List(MapType(List(IntType.datatype, DoubleType.datatype), List(TextType.datatype)))))
    }
  }
}
