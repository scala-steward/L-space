package lspace.provider.wrapped

import lspace.structure._

abstract class WrappedEdge[S, E](override val self: Edge[S, E]) extends Edge[S, E] with WrappedResource[Edge[S, E]] {

  def key: Property              = self.key
  override val value: Edge[S, E] = self.value
  //  override def addType(classType: Property): Unit = self.addType(classType)

  def to: Resource[E]   = self.to
  def from: Resource[S] = self.from
}
