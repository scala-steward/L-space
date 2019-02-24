package lspace.lgraph.provider.file

/**
  * helper for building a graph from .303 files
  * @param nodeIds
  * @param edgeIds
  * @param valueIds
  */
case class IdMaps(nodeIds: Map[Long, Long] = Map[Long, Long](),
                  edgeIds: Map[Long, Long] = Map[Long, Long](),
                  valueIds: Map[Long, Long] = Map[Long, Long]()) {
  def ++(idMaps: IdMaps): IdMaps =
    IdMaps(nodeIds ++ idMaps.nodeIds, edgeIds ++ idMaps.edgeIds, valueIds ++ idMaps.valueIds)
}
