package lspace
package zio

import _root_.zio._
import _root_.zio.stream._

extension (node: Node)
  def labels: ZStream[Graph, Throwable, String] = ZStream.succeed("something")
