package lspace.lgraph

import lspace.librarian.structure.{DataType, Value}

object LValue {
  def apply[T](id: Long, value: T, label: DataType[T], graph: LGraph): graph._Value[T] with LValue[T] = {
    val _id    = id
    val _value = value
    val _label = label
    val _graph = graph
    new graph._Value[T] with LValue[T] {
      val id    = _id
      val value = _value
      val label = _label
      val graph = _graph
    }
  }
}

trait LValue[T] extends LResource[T] with Value[T] {}
