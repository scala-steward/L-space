package lspace.librarian.provider.mem

import lspace.librarian.structure._

object MemValue {
//  protected[provider] def apply[T](_value: T, dt: DataType[T])(implicit _graph: MemGraph): MemValue[T] =
//    new MemValue[T] {
//      val value: T                 = _value
//      val label: DataType[T]       = dt
//      implicit val graph: MemGraph = _graph
//
//      val id: Long = graph.idGenerator.next
//    }

  protected[provider] def apply[T](_value: T, dt: DataType[T], _id: Long)(implicit _graph: MemGraph): MemValue[T] =
    new MemValue[T] {
      val value: T              = _value
      val label: DataType[T]    = dt
      implicit val graph: Graph = _graph

      val id: Long = _id
    }
}

trait MemValue[T] extends MemResource[T] with Value[T] {

  override def remove(): Unit = {
    super.remove()
  }
}
