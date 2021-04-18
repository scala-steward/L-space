package lspace.provider.transaction

import lspace.provider.mem.MemEdge
import lspace.structure.Edge

trait TEdge[S, E] extends MemEdge[S, E] with TResource[Edge[S, E]]
