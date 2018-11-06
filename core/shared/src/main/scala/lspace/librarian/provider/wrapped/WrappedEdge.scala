package lspace.librarian.provider.wrapped

import lspace.librarian.structure._
import lspace.util.CacheStatus

abstract class WrappedEdge[S, E](override val self: Edge[S, E]) extends Edge[S, E] with WrappedResource[Edge[S, E]] {

  def key: Property              = self.key
  override val value: Edge[S, E] = self.value
  //  override def addType(classType: Property): Unit = self.addType(classType)

  def inV: Resource[E]  = self.inV
  def outV: Resource[S] = self.outV
}
