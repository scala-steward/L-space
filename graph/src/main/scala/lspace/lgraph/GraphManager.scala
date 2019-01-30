package lspace.lgraph

import monix.execution.CancelableFuture

abstract class GraphManager[G <: LGraph](val graph: G) {

  def idProvider: LGraphIdProvider

  def close(): CancelableFuture[Unit]
}
