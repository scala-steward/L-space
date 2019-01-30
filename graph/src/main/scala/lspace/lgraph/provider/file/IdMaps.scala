package lspace.lgraph.provider.file

case class IdMaps(nodeIds: Map[Long, Long] = Map[Long, Long](),
                  edgeIds: Map[Long, Long] = Map[Long, Long](),
                  valueIds: Map[Long, Long] = Map[Long, Long]()) {
  def ++(idMaps: IdMaps): IdMaps =
    IdMaps(nodeIds ++ idMaps.nodeIds, edgeIds ++ idMaps.edgeIds, valueIds ++ idMaps.valueIds)
}
