package lspace.lgraph

import lspace.structure.NameSpaceGraph
import monix.eval.Task
import monix.execution.CancelableFuture

trait LNSGraph extends LDataGraph with NameSpaceGraph {
  def index: LIndexGraph
  def graph: LGraph
}
