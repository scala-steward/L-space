package lspace.datatype

import lspace.structure.Property

object DataTypeDef {
  implicit def dDefToDataType[T <: DataType[_]](df: DataTypeDef[T]): T = df.datatype
}

trait DataTypeDef[T <: DataType[_]] {

  def datatype: T
  def iri   = datatype.iri
  def iris  = datatype.iris
  def label = datatype.label

  def keys: Object
  def properties: List[Property] = List()
  trait Properties {}
}
