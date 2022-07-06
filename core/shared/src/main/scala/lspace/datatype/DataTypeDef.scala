package lspace.datatype

import lspace.structure.Property

object DataTypeDef {
  implicit def dDefToDataType[T <: DataType[_]](df: DataTypeDef[T]): T = df.datatype
  trait Properties {}
}

trait DataTypeDef[T <: DataType[Any]] {

  def datatype: T
  def iri: String       = datatype.iri
  def iris: Set[String] = datatype.iris
  def label             = datatype.label

  def keys: Object
  def properties: List[Property] = List()
}
