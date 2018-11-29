package lspace.librarian.provider.transaction

import lspace.librarian.provider.mem.MemEdge
import lspace.librarian.structure.{Edge, Graph}

trait TEdge[S, E] extends MemEdge[S, E] with TResource[Edge[S, E]]
