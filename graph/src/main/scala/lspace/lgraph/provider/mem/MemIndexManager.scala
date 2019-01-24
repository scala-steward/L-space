package lspace.lgraph.provider.mem

import lspace.lgraph.LGraph
import lspace.lgraph.index.IndexManager

class MemIndexManager[G <: LGraph](graph: G) extends IndexManager(graph) {}
