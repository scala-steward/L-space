package lspace.util

import lspace.client.io.HttpClient
import lspace.codec.json.{JsonDecoder, JsonEncoder}
import lspace.lgraph.LGraph
import lspace.lgraph.provider.file.FileStoreProvider
import lspace.lgraph.provider.mem.MemIndexProvider
import lspace.structure.Graph

case class FileGraphConfig(name: String, path: String) extends GraphConfig {
  def toGraph[T](implicit encoder: JsonEncoder[T], decoder: JsonDecoder[T], httpClient: HttpClient): Graph =
    LGraph(FileStoreProvider(name, path), new MemIndexProvider())
}
