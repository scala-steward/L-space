package lspace.provider.wrapped

import lspace.structure._
import lspace.util.CacheStatus

abstract class WrappedNode(override val self: Node) extends Node with WrappedResource[Node] {

  override val value: Node = self.value
  //  @transient override val status: CacheStatus.CacheStatus = self.status
  //  @transient override val memento: Observable[Long] = self.memento

  override def labels: List[Ontology]                 = self.labels
  override def addLabel(classType: Ontology): Unit    = self.addLabel(classType)
  override def removeLabel(classType: Ontology): Unit = self.removeLabel(classType)
}
