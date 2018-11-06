package lspace.librarian.provider.mem

import lspace.librarian.structure._

object MemValue {
  protected[provider] def apply[T](_value: T, dt: DataType[T])(implicit _graph: MemGraph): MemValue[T] =
    new MemValue[T] {
      val value: T                 = _value
      val label: DataType[T]       = dt
      implicit val graph: MemGraph = _graph
    }
}

trait MemValue[T] extends MemResource[T] with Value[T] {

  override def remove(): Unit = {
    super.remove()
    graph.valueIndex -= label -> value
    graph.valueCache -= id
  }
}
