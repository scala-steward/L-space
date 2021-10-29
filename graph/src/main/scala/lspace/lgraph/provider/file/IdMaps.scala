package lspace.lgraph.provider.file

import scala.collection.immutable.HashMap

/** helper for building a graph from .303 files
  *
  * @param nodeIds
  * @param edgeIds
  * @param valueIds
  */
case class IdMaps(
  nodeIds: HashMap[Long, Long] = HashMap[Long, Long](),
  edgeIds: HashMap[Long, Long] = HashMap[Long, Long](),
  valueIds: HashMap[Long, Long] = HashMap[Long, Long]()
) {
  def ++(idMaps: IdMaps): IdMaps =
    IdMaps(nodeIds ++ idMaps.nodeIds, edgeIds ++ idMaps.edgeIds, valueIds ++ idMaps.valueIds)
}
