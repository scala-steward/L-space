package lspace.util

import lspace.codec.{NativeTypeDecoder, NativeTypeEncoder}
import lspace.lgraph.LGraph
import lspace.lgraph.provider.file.FileStoreProvider
import lspace.lgraph.provider.mem.MemIndexProvider
import lspace.structure.Graph

case class FileGraphConfig(name: String, path: String) extends GraphConfig {
  def toGraph[T](implicit encoder: NativeTypeEncoder.Aux[T], decoder: NativeTypeDecoder.Aux[T]): Graph =
    LGraph(FileStoreProvider(name, path), new MemIndexProvider())
}
